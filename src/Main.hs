{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Telegram.Bot.API (updateChatId, ParseMode (..), User (..), SomeReplyMarkup (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Data.Proxy
import qualified Telegram.Bot.API as TG
import Servant.Client
import System.Environment (getEnv)
import Telegram.Bot.AppM (authViaTelegram, currentUser, AppM (..), AppEnv (..))
import GHC.Float (divideDouble)
import Control.Monad.Reader (ReaderT(..), asks)
import Control.Monad.State (evalStateT)

import Telegram.Bot.FSAfe
import SmartPrimitives.TextLenRange (unTextLenRange, TextLenRange (..))
import Data.Text (Text)
import Models
import Data.List.NonEmpty (NonEmpty (..))
import CheckCheck.Contracts.Users
import Clients.AuthService
import Servant.API (WithStatus)
import CheckCheck.Contracts.Users.Contacts (CreateContactReqBody(..))
import SmartPrimitives.TextMaxLen (TextMaxLen(..), mkTextMaxLen, unTextMaxLen)
import CheckCheck.Contracts.Receipts (ReceiptResp(..))
import Data.UUID (UUID)
import CheckCheck.Contracts.Users.OutgoingRequests (SendRequestReqBody(..), IndexSelectionReqBody (..))
import Optics hiding (indices)
import Clients.Backend
import Clients.Utils
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NonEmpty
import Control.Arrow ((&&&))
import GHC.Generics (Generic)
import Control.Monad.Except (runExceptT)
import Data.List (sortOn, find, sort, (!?))
import Control.Monad.IO.Class (liftIO)
import Data.Time (showGregorian, UTCTime (utctDay))
import CheckCheck.Contracts.Users.IncomingRequests (CompleteIncomingRequestReqBody(..), CompleteIncomingRequestResp (..))

data InitialState = InitialState
instance StateMessage InitialState where
  stateMessage _ = Send $
    textMessage (T.unlines
    [ "*Main commands:*"
    , "/qr \\<qr\\> \\- View a receipt"
    , "/contact \\<\\[@\\]username\\> \\- Add contact"
    , "/requests \\- See all incoming requests"
    ])
    & withParseMode MarkdownV2

data ViewingReceipt = ViewingReceipt Text [ReceiptItem]
instance StateMessage ViewingReceipt where
  stateMessage (ViewingReceipt _ items') = Edit $
    textMessage (T.unlines
    $ "Scanned receipt items: "
    : map formatReceiptItem items)
    & withInlineKeyboard
      (row [button "Ok" Cancel, button "Select Items" StartSelectingReceiptItems])
    where
      items = sort items'

data SelectingReceiptItems0 = SelectingReceiptItems0 Text [ReceiptItem]
instance StateMessage SelectingReceiptItems0 where
  stateMessage (SelectingReceiptItems0 _ items') = Edit $
    textMessage ("Selected receipt items: " <> tshow ([] @Int))
    & withInlineKeyboard
      [ col $ map toAddReceiptItemButton items
      , single $ button "Cancel"  Cancel
      ]
    where
      items = sort items'
      toAddReceiptItemButton item =
        let (i, prettyItem) = view #index &&& formatReceiptItem $ item
        in button prettyItem (AddReceiptItemCallbackData i)

data SelectingReceiptItems = SelectingReceiptItems Text ReceiptItem [(Bool, ReceiptItem)]
instance StateMessage SelectingReceiptItems where
  stateMessage (SelectingReceiptItems _ item0 items') = Edit $
    textMessage ("Selected receipt items: " <> tshow selectedIndices)
    & withInlineKeyboard
      [ col $ map toToggleReceiptItemButton items
      , row [ button "Cancel" Cancel
            , button "Confirm" StartSelectingRequestRecipient
            ]
      ]
    where
      items = sortOn (view _2) $ (True, item0) : items'
      selectedIndices = map (view $ _2 % #index) $ filter fst items
      toToggleReceiptItemButton (isAdded, item) =
        let (i, prettyItem) = view #index &&& formatReceiptItem $ item
        in if isAdded
          then button prettyItem (RemoveReceiptItemCallbackQuery i)
          else button prettyItem (AddReceiptItemCallbackData i)

data SelectingRequestRecipient =
  SelectingRequestRecipient Text (NonEmpty ReceiptItem) [UserContact]
instance StateMessage SelectingRequestRecipient where
  stateMessage (SelectingRequestRecipient _ items contacts) = Send $
      textMessage (T.unlines
          $ ("В сумме на: " <> tshow receiptTotal)
          : (view #name <$> NonEmpty.toList items))
      & withInlineKeyboard
        [ col $ map toSelectRequestRecipientButton contacts
        , single $ button "Cancel" Cancel
        ]
    where
      receiptTotal = ((`divideDouble` 100) . fromInteger) $ sum $ items <&> view #itemTotal

      toSelectRequestRecipientButton
        UserContact{contactUsername = TextLenRange contactUsername, mContactName, contactUserId} =
        let name = maybe contactUsername unTextMaxLen mContactName
        in button name (SelectRequestRecipient contactUserId)

data ViewingRequests = ViewingRequests
  { requests :: [Request]
  , showPending :: Bool
  }
instance StateMessage ViewingRequests where
  stateMessage ViewingRequests{requests, showPending} = Edit $
    textMessage "All incoming requests: "
    & withInlineKeyboard
      [ col $ zipWith (curry toShowRequestButton) [0..] filteredRequests
      , single $ button "Ok" Cancel
      ]
    where
      filteredRequests = if showPending
        then requests
        else filter isPending requests

      toShowRequestButton (i, req) = button (formatRequest req) (ShowRequestCallback i)

data ViewingRequest = ViewingRequest Request ViewingRequests
instance StateMessage ViewingRequest where
  stateMessage (ViewingRequest req _) = Edit $
    textMessage (T.unlines
    $ (if req ^. #isPending then "Request is pending 👻" else "Request is done ✅")
    : map formatRequestItem (req ^.. #items))
    & withInlineKeyboard
      [ row $ if req ^. #isPending
          then [ button ("Pay " <> tshow (req ^. #requestTotal)) PayForRequest
               , button "Mark Done" MarkRequestCompleted
               ]
          else []
      , single $ button "Back" Cancel
      ]
    where
      formatRequestItem RequestItem{identity, price, quantity} =
        identity <> " x " <> tshow price <> " = " <> tshow quantity <> " rub"

data Start = Start
  deriving (Generic, IsUnit)
  deriving ParseTransition via CommandUnit "start" Start
instance HandleTransitionM Start InitialState InitialState AppM where
  handleTransitionM Start InitialState = do
    token <- authViaTelegram =<< currentUser
    UserResp{username} <- runReq $ getMe token
    sendText_ $ "Nice to see you, " <> unTextLenRange username
    return InitialState

data AddContact = AddContact UserQuery (Maybe (TextMaxLen 50))
instance ParseTransition AddContact where
  parseTransition = do
    txt <- command "contact"
    let (baseUsername, contactNamePart) = T.span (/= ' ') txt
        contactNameRaw = T.strip contactNamePart
    userQuery <- case T.uncons baseUsername of
      Nothing -> fail "empty username"
      Just ('@', rest) -> pure $ UserTgUsernameQuery rest
      Just _ ->           pure $ UserUsernameQuery txt
    contactNameResult <- case contactNameRaw of
      "" -> pure Nothing
      name -> case mkTextMaxLen name of
        Just validName -> pure $ Just validName
        Nothing -> fail "contact name is too long, 50 symbols is the max length"
    return $ AddContact userQuery contactNameResult
instance HandleTransitionM AddContact InitialState InitialState AppM where
  handleTransitionM (AddContact userQuery contactName) InitialState = do
    token <- authViaTelegram =<< currentUser
    u <- runReq $ getUser token userQuery
    if| Just AuthServiceUser{userId} <- matchUnion @AuthServiceUser u ->
        handleSuccess token userId
      | Just _ <- matchUnion @(WithStatus 404 ()) u ->
        handleUserNotFound
      | otherwise -> return ()
    return InitialState
    where
      formattedUsername = case userQuery of
        UserUserIdQuery uuid -> tshow uuid
        UserUsernameQuery username -> username
        UserTgUserIdQuery int -> tshow int
        UserTgUsernameQuery tgUsername -> T.cons '@' tgUsername

      handleSuccess token contactUserId = do
        runReq_ $ createContact token CreateContactReqBody{..}
        sendText_ $ case contactName of
          Just (TextMaxLen name) ->
            "contact " <> formattedUsername <> " successfully added as " <> name
          Nothing ->
            "contact " <> formattedUsername <> " successfully added"

      handleUserNotFound = case userQuery of
        UserTgUsernameQuery{} -> do
          mBotName <- liftBot $ asks $ userUsername . botContextUser
          case mBotName of
            Just botName -> do
              sendText_ $ T.unlines
                [ userIsNotRegisteredLine
                , "Send them the following link to join:"
                ]
              sendText_ $ "https://t.me/" <> botName <> "?start=start"
            Nothing ->
              sendText_ userIsNotRegisteredLine
        _ ->
          sendText_ userIsNotRegisteredLine
        where
          userIsNotRegisteredLine =
            "User " <> formattedUsername <> " is not registered in check-check"

newtype ShowReceipt = ShowReceipt Text
  deriving ParseTransition via Command "qr"
instance RunReq BackendClientM m => HandleTransitionM ShowReceipt InitialState ViewingReceipt m where
  handleTransitionM (ShowReceipt qr) InitialState = do
    ReceiptResp respItems <- runReq $ getReceipt qr
    let items = fromResp <$> respItems
    return $ ViewingReceipt qr items

data StartSelectingReceiptItems = StartSelectingReceiptItems
  deriving (Read, Show)
  deriving IsCallbackQuery via ReadShow StartSelectingReceiptItems
  deriving ParseTransition via CallbackQueryData StartSelectingReceiptItems
instance HandleTransition StartSelectingReceiptItems ViewingReceipt SelectingReceiptItems0 where
  handleTransition StartSelectingReceiptItems (ViewingReceipt qr items) =
    SelectingReceiptItems0 qr items

newtype AddReceiptItemCallbackData = AddReceiptItemCallbackData Int
  deriving (Read, Show)
  deriving IsCallbackQuery via ReadShow AddReceiptItemCallbackData
newtype AddReceiptItem = AddReceiptItem ReceiptItem
instance ParseTransitionFrom SelectingReceiptItems0 AddReceiptItem where
  parseTransitionFrom (SelectingReceiptItems0 _ items) = do
    AddReceiptItemCallbackData i <- callbackQueryDataRead
    case find ((i ==) . view #index) items of
      Nothing -> fail ""
      Just item -> return $ AddReceiptItem item
instance HandleTransition AddReceiptItem SelectingReceiptItems0 SelectingReceiptItems where
  handleTransition (AddReceiptItem item) (SelectingReceiptItems0 qr items) =
    let items' = map (False,) $ filter (item /=) items
    in SelectingReceiptItems qr item items'

instance ParseTransitionFrom SelectingReceiptItems AddReceiptItem where
  parseTransitionFrom (SelectingReceiptItems _ _ items) = do
    AddReceiptItemCallbackData i <- callbackQueryDataRead
    case find ((i ==) . view #index) $ map snd items of
      Nothing -> fail ""
      Just item -> return $ AddReceiptItem item
instance HandleTransition AddReceiptItem SelectingReceiptItems SelectingReceiptItems where
  handleTransition (AddReceiptItem item) (SelectingReceiptItems qr item0 initialItems) =
    let items = initialItems & traversed % unsafeFiltered ((item ==) . view _2) % _1 .~ True
    in SelectingReceiptItems qr item0 items --

newtype RemoveReceiptItemCallbackQuery = RemoveReceiptItemCallbackQuery Int
  deriving (Read, Show)
  deriving IsCallbackQuery via ReadShow RemoveReceiptItemCallbackQuery

newtype RemoveReceiptItem = RemoveReceiptItem Int
instance ParseTransitionFrom SelectingReceiptItems RemoveReceiptItem where
  parseTransitionFrom (SelectingReceiptItems _ item0 _) = do
    RemoveReceiptItemCallbackQuery i <- callbackQueryDataRead
    if i == item0 ^. #index
      then fail ""
      else return $ RemoveReceiptItem i
instance HandleTransition RemoveReceiptItem SelectingReceiptItems SelectingReceiptItems where
  handleTransition (RemoveReceiptItem i) (SelectingReceiptItems qr item0 items) =
    let items' = items & traversed % unsafeFiltered ((i ==) . (^. _2 % #index)) % _1 .~ False
    in SelectingReceiptItems qr item0 items' --

newtype ReplaceReceiptItem0 = ReplaceReceiptItem0 ReceiptItem
instance ParseTransitionFrom SelectingReceiptItems ReplaceReceiptItem0 where
  parseTransitionFrom (SelectingReceiptItems _ item0 items) = do
    RemoveReceiptItemCallbackQuery i <- callbackQueryDataRead
    if i /= item0 ^. #index
      then fail ""
      else case snd <$> find fst items of
        Nothing -> fail ""
        Just item0' -> return $ ReplaceReceiptItem0 item0'
instance HandleTransition ReplaceReceiptItem0 SelectingReceiptItems SelectingReceiptItems where
  handleTransition (ReplaceReceiptItem0 newItem0) (SelectingReceiptItems qr item0 items) =
    let items' = (False, item0) : filter ((newItem0 /=) . view _2) items
    in SelectingReceiptItems qr newItem0 items' --

data RemoveAllReceiptItems = RemoveAllReceiptItems
instance ParseTransitionFrom SelectingReceiptItems RemoveAllReceiptItems where
  parseTransitionFrom (SelectingReceiptItems _ item0 items) = do
    RemoveReceiptItemCallbackQuery i <- callbackQueryDataRead
    if i == item0 ^. #index && not (any fst items)
      then return RemoveAllReceiptItems
      else fail ""
instance HandleTransition RemoveAllReceiptItems SelectingReceiptItems SelectingReceiptItems0 where
  handleTransition RemoveAllReceiptItems (SelectingReceiptItems qr item0 items) =
    SelectingReceiptItems0 qr (item0 : map snd items)

data StartSelectingRequestRecipient = StartSelectingRequestRecipient
  deriving (Read, Show)
  deriving IsCallbackQuery via ReadShow StartSelectingRequestRecipient
  deriving ParseTransition via CallbackQueryData StartSelectingRequestRecipient
instance HandleTransitionM StartSelectingRequestRecipient
  SelectingReceiptItems SelectingRequestRecipient AppM where
  handleTransitionM StartSelectingRequestRecipient (SelectingReceiptItems qr item0 items0) = do
    let items = item0 :| map snd (filter fst items0)
    let itemsList = NonEmpty.toList items
    let msgTxt = T.unlines
          $ ("Selected receipt items: " <> tshow (view #index <$> itemsList))
          : map (formatReceiptItem . snd) items0
    editUpdateMessage_ $ textMessage msgTxt
    token <- authViaTelegram =<< currentUser
    contactsResp <- runReq $ getContacts token
    let contacts = fromResp <$> contactsResp
    return $ SelectingRequestRecipient qr items contacts

newtype SelectRequestRecipient = SelectRequestRecipient UUID
  deriving (Read, Show)
  deriving IsCallbackQuery via ReadShow SelectRequestRecipient
  deriving ParseTransition via CallbackQueryData SelectRequestRecipient
instance HandleTransitionM SelectRequestRecipient SelectingRequestRecipient InitialState AppM where
  handleTransitionM
    (SelectRequestRecipient recipientId)
    (SelectingRequestRecipient receiptQr items _)
    = do
    let indices = items <&> view #index
    token <- authViaTelegram =<< currentUser
    let reqBody = SendReceiptItemsRequestReqBody
          {receiptQr, indexSelections = NonEmpty.singleton IndexSelectionReqBody{..}}
    runReq_ $ sendRequest token reqBody
    sendText_ "Request successfully sent"
    return InitialState

data ShowRequests = ShowRequests
  deriving (Generic, IsUnit)
  deriving ParseTransition via CommandUnit "requests" ShowRequests
instance HandleTransitionM ShowRequests InitialState ViewingRequests AppM where
  handleTransitionM ShowRequests InitialState = do
    token <- authViaTelegram =<< currentUser
    requests <- map fromResp <$> runReq (getIncomingRequests token)
    return ViewingRequests{requests, showPending = True}

newtype ShowRequestCallback = ShowRequestCallback Int
  deriving (Read, Show)
  deriving IsCallbackQuery via ReadShow ShowRequestCallback
newtype ShowRequest = ShowRequest Request
  deriving (Read, Show)
instance ParseTransitionFrom ViewingRequests ShowRequest where
  parseTransitionFrom (ViewingRequests requests _) = do
    ShowRequestCallback i <- callbackQueryDataRead
    requests !? i & maybe (fail "how!?") (return . ShowRequest)
instance HandleTransition ShowRequest ViewingRequests ViewingRequest where
  handleTransition (ShowRequest req) state@ViewingRequests{} = ViewingRequest req state

data PayForRequest = PayForRequest
  deriving (Read, Show)
  deriving IsCallbackQuery via ReadShow PayForRequest
  deriving ParseTransition via CallbackQueryData PayForRequest
instance HandleTransitionM PayForRequest ViewingRequest InitialState AppM where
  handleTransitionM PayForRequest (ViewingRequest Request{requestId} _) = do
    token <- authViaTelegram =<< currentUser
    let reqBody = PayForReqBody {roundingStrategy=Nothing, roundingEps=Nothing}
    resp <- runReq $ completeIncomingRequest token requestId reqBody
    editUpdateMessageReplyMarkup_ $ SomeInlineKeyboardMarkup $ col []
    let msgTxt = case resp of
          PayedForResp budgetResp ->
            let Budget{isLowerBoundExceeded, amount} = fromResp budgetResp
            in if isLowerBoundExceeded
              then "Your budget lower bound is exceeded: " <> tshow amount
              else "Your current budget: " <> tshow amount

          _ -> "Something went wrong"
    sendText_ msgTxt
    return InitialState

data MarkRequestCompleted = MarkRequestCompleted
  deriving (Read, Show)
  deriving IsCallbackQuery via ReadShow MarkRequestCompleted
  deriving ParseTransition via CallbackQueryData MarkRequestCompleted
instance HandleTransitionM MarkRequestCompleted ViewingRequest InitialState AppM where
  handleTransitionM MarkRequestCompleted (ViewingRequest Request{requestId} _) = do
    token <- authViaTelegram =<< currentUser
    resp <- runReq $ completeIncomingRequest token requestId MarkCompletedReqBody
    editUpdateMessageReplyMarkup_ $ SomeInlineKeyboardMarkup $ col []
    let msgTxt = case resp of
          MarkedCompletedResp -> "Request successfully marked completed"
          _ -> "Something went wrong"
    sendText_ msgTxt
    return InitialState

data Cancel = Cancel
  deriving (Read, Show)
  deriving IsCallbackQuery via ReadShow Cancel
  deriving ParseTransition via CallbackQueryData Cancel
instance MonadBot m => HandleTransitionM Cancel ViewingReceipt InitialState m where
  handleTransitionM Cancel _ = do
    editUpdateMessageReplyMarkup_ $ SomeInlineKeyboardMarkup $ col []
    return InitialState
instance MonadBot m => HandleTransitionM Cancel SelectingReceiptItems0 InitialState m where
  handleTransitionM Cancel (SelectingReceiptItems0 _ items) = do
    let msgTxt = T.unlines $ "Scanned receipt items: " : map formatReceiptItem items
    editUpdateMessage_ $ textMessage msgTxt
    return InitialState
instance MonadBot m => HandleTransitionM Cancel SelectingReceiptItems InitialState m where
  handleTransitionM Cancel (SelectingReceiptItems qr item0 items') =
    let items = item0 : map snd items'
    in handleTransitionM Cancel (SelectingReceiptItems0 qr items)

instance MonadBot m => HandleTransitionM Cancel SelectingRequestRecipient InitialState m where
  handleTransitionM Cancel SelectingRequestRecipient{} = do
    editUpdateMessageReplyMarkup_ $ SomeInlineKeyboardMarkup $ col []
    return InitialState

instance MonadBot m => HandleTransitionM Cancel ViewingRequests InitialState m where
  handleTransitionM Cancel ViewingRequests{} = do
    editUpdateMessageReplyMarkup_ $ SomeInlineKeyboardMarkup $ col []
    return InitialState

instance HandleTransition Cancel ViewingRequest ViewingRequests where
  handleTransition Cancel (ViewingRequest _ state) = state

data RunConfig = RunConfig
  { authApiKey :: String
  , tgToken :: TG.Token
  , beClientBaseUrl :: BaseUrl
  , authClientBaseUrl :: BaseUrl
  }

type FSA =
 '[ '(InitialState, '[Start, AddContact, ShowReceipt, ShowRequests])
  , '(ViewingReceipt, '[StartSelectingReceiptItems, Cancel])
  , '(SelectingReceiptItems0, '[AddReceiptItem, Cancel])
  , '(SelectingReceiptItems,
     '[ AddReceiptItem
      , RemoveReceiptItem, ReplaceReceiptItem0, RemoveAllReceiptItems
      , StartSelectingRequestRecipient
      , Cancel
      ])
  , '(SelectingRequestRecipient, '[SelectRequestRecipient, Cancel])
  , '(ViewingRequests, '[ShowRequest, Cancel])
  , '(ViewingRequest, '[MarkRequestCompleted, PayForRequest, Cancel])
  ]

run :: RunConfig -> IO ()
run RunConfig{..} = do
  manager <- newManager defaultManagerSettings
  let backendClientEnv = mkClientEnv manager   beClientBaseUrl
  let authClientEnv    = mkClientEnv manager authClientBaseUrl

  hoistStartKeyedBot_ (Proxy @FSA) (nt AppEnv{..}) updateChatId InitialState tgToken
  where nt env (AppM app) = app `runReaderT` env `evalStateT` Nothing & runExceptT >>= \case
          Right a -> pure a
          Left err -> do
            liftIO $ print err
            fail $ show err

main :: IO ()
main = do
  putStrLn "The bot is running"
  authApiKey <- getEnv "AUTH_API_KEY"
  tgToken <- getEnvToken "TELEGRAM_BOT_TOKEN"
  beClientBaseUrl <- parseBaseUrl =<< getEnv "BACKEND_BASE_URL"
  authClientBaseUrl <- parseBaseUrl =<< getEnv "AUTH_BASE_URL"
  run RunConfig{..}

tshow :: Show a => a -> T.Text
tshow = T.pack . show

formatReceiptItem :: ReceiptItem -> T.Text
formatReceiptItem item@ReceiptItem{index, name, quantity}
  =  tshow index <> ". "
  <> T.take 20 name <> "... x "
  <> tshow quantity <> " = "
  <> tshow priceSum <> " rub"
  where
  priceSum = view #itemTotal item `divide` 100 :: Double
    where divide a b = fromIntegral a / b

formatRequest :: Request -> Text
formatRequest req
  =   T.pack (showGregorian $ utctDay $ req ^. #createdAt)
  <> ": "
  <> tshow priceSum
  <> " rub"
  <> if req ^. #isPending then "" else " ✅"
  where
  priceSum = (req ^. #requestTotal) `divide` 100 :: Double
    where divide a b = fromIntegral a / b

