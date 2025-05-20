{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveAnyClass #-}

module Telegram.Bot.FSA
  ( InitialState(..), ShowReceipt, AddContact
  , ViewingReceipt, SelectReceiptItem
  , SelectingReceiptItems, StartSelectingRequestRecipient
  , SelectingRequestRecipient, SelectRequestRecipient
  , Cancel, Start
  ) where

import Data.Text (Text)
import Data.UUID (UUID)

import Data.List.NonEmpty (NonEmpty, nonEmpty, toList, singleton)

import Models (ReceiptItem(..), FromResp (fromResp))
import Telegram.Bot.FSAfe (IsState(..), MessageContext (MessageContext), HandleTransitionM (..), HandleTransition (..), ParseTransition(..), CommandUnit(..), Command, Command'(..), command, CallbackQueryData(..))
import Telegram.Bot.AppM (AppM, authViaTelegram, currentUser, AppError (CancelTransition))
import GHC.Generics (Generic)
import Telegram.Bot.DSL (AsMessage, IsCallbackData, ReadShow(..), (:\), CallbackButtons, F, let', andLet, UnitCallbackBtn, IsUnit (..), (:|:))
import Optics hiding (indices)
import CheckCheck.Contracts.Receipts (ReceiptResp(..))
import Clients.Utils (runReq, runReq_)
import Clients.Backend (getReceipt, sendRequest, createContact)
import CheckCheck.Contracts.Users.OutgoingRequests (SendRequestReqBody(..), IndexSelectionReqBody (..))
import Clients.AuthService (AuthServiceUser(..), UserQuery (..), getUser)
import Servant.API (WithStatus)
import CheckCheck.Contracts.Users.Contacts (CreateContactReqBody(..))
import SmartPrimitives.TextMaxLen (mkTextMaxLen, TextMaxLen (..))
import qualified Data.Text as T
import Servant.Client (matchUnion)
import Telegram.Bot.FSAfe.Reply (replyText)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (ReaderT(..))

data InitialState = InitialState
  deriving Generic

data ViewingReceipt = ViewingReceipt
  { qr :: Text
  , items :: [ReceiptItem]
  } deriving Generic

data SelectingReceiptItems = SelectingReceiptItems
  { qr :: Text
  , items :: [(Bool, ReceiptItem)]
  } deriving Generic

data SelectingRequestRecipient = SelectingRequestRecipientD
  { qr :: Text
  , items :: NonEmpty ReceiptItem
  } deriving Generic

newtype ShowReceipt = ShowReceipt Text
  deriving ParseTransition via Command "qr"

data UsernameType = Telegram | Regular
data AddContact = AddContact UsernameType Text

data Start = Start
  deriving (Generic, IsUnit)
  deriving ParseTransition via CommandUnit "start" Start

data Cancel = Cancel
  deriving (Read, Show, Generic, IsUnit)
  deriving IsCallbackData via ReadShow Cancel
  deriving ParseTransition via CallbackQueryData Cancel

newtype SelectReceiptItem = SelectReceiptItem { index :: Int }
  deriving (Read, Show, Generic)
  deriving IsCallbackData via ReadShow SelectReceiptItem
  deriving ParseTransition via CallbackQueryData SelectReceiptItem

data StartSelectingRequestRecipient = StartSelectingRequestRecipient
  deriving (Read, Show, Generic, IsUnit)
  deriving IsCallbackData via ReadShow StartSelectingRequestRecipient
  deriving ParseTransition via CallbackQueryData StartSelectingRequestRecipient

newtype SelectRequestRecipient = SelectRequestRecipient UUID
  deriving (Read, Show)
  deriving IsCallbackData via ReadShow SelectRequestRecipient
  deriving ParseTransition via CallbackQueryData SelectRequestRecipient

instance IsState InitialState AppM where
  type StateMessage InitialState
    = AsMessage "В будущем тут появится свой текст"

instance ParseTransition AddContact where
  parseTransition = do
       t <- command "contact"
       return $ case T.uncons t of
         Just ('@', contact) -> AddContact Telegram contact
         _                   -> AddContact Regular t

instance IsState ViewingReceipt AppM where
  type StateMessage ViewingReceipt
    =  "Scanned receipt items: "
    :\ CallbackButtons
         (F"{show index}. {name} x {show quantity} = {show priceSum} rub")
         SelectReceiptItem "items" -- TODO btw this has to be vertical
    :\ UnitCallbackBtn "Ok" Cancel

  extractMessageContext ViewingReceipt{items} = do
    return $ MessageContext -- TODO this is bad obviously
      $ let'   @"items" (SelectReceiptItem . view #index <$> items)
      $ let'   @"quantity" ()
      $ let'   @"priceSum" ()
      $ andLet @"name" "Daniel"

instance IsState SelectingReceiptItems AppM where
  -- TODO actually this should have different StateMessage
  type StateMessage SelectingReceiptItems
    =  F"Selected receipt items: {show selectedItems}"
    :\ CallbackButtons (F"{show index}. {name} x {show quantity} = {show priceSum} rub")
       SelectReceiptItem "items" -- TODO btw this has to be vertical
    :\ UnitCallbackBtn "Confirm" StartSelectingRequestRecipient :|: UnitCallbackBtn "Cancel" Cancel
    --                            ^ maybe change to polymorphic Confirm?

  extractMessageContext SelectingReceiptItems{items} =
    pure $ MessageContext -- TODO this is bad obviously
      $ let'   @"selectedItems" (items & filter fst & map (^. _2 % #index))
      $ let'   @"items" (SelectReceiptItem . (^. _2 % #index) <$> items)
      $ let'   @"quantity" ()
      $ let'   @"priceSum" ()
      $ andLet @"name" "Daniel"

instance IsState SelectingRequestRecipient AppM where
  type StateMessage SelectingRequestRecipient
    =  F"Selected receipt items: {show items}"
    :\ CallbackButtons (F"{show index}. {name} x {show quantity} = {show priceSum} rub")
       SelectReceiptItem "itemsButtons" -- TODO btw this has to be vertical
       -- TODO buttons above are copied
    :\ UnitCallbackBtn "Confirm" StartSelectingRequestRecipient :|: UnitCallbackBtn "Cancel" Cancel

  extractMessageContext SelectingRequestRecipientD{items} = do
    return $ MessageContext -- TODO this is bad obviously
      $ let'   @"items" items
      $ let'   @"itemsButtons" (toList $ SelectReceiptItem . view #index <$> items)
      $ let'   @"quantity" () -- ^ TODO would be great if the thing accepted Foldable, not just lists
      $ let'   @"priceSum" ()
      $ andLet @"name" "Daniel"

instance HandleTransitionM ShowReceipt InitialState ViewingReceipt AppM where
  handleTransitionM (ShowReceipt qr) InitialState = do
    ReceiptResp respItems <- runReq $ getReceipt qr
    let items = fromResp <$> respItems
    return $ ViewingReceipt qr items

instance HandleTransitionM AddContact InitialState InitialState AppM where
  handleTransitionM t InitialState = addContact t

instance HandleTransitionM Start InitialState InitialState AppM where
  handleTransitionM Start InitialState = do
    replyText "/start"
    return InitialState

instance HandleTransition Cancel a InitialState where
  handleTransition Cancel _ = InitialState

instance HandleTransition SelectReceiptItem ViewingReceipt SelectingReceiptItems where
  handleTransition (SelectReceiptItem i) ViewingReceipt{items, ..} =
    SelectingReceiptItems
      { items = items
          <&> (False,)
          & traversed % unsafeFiltered ((i ==) . (^. _2 % #index)) % _1 .~ True
      , ..}

instance HandleTransition SelectReceiptItem SelectingReceiptItems SelectingReceiptItems where
  handleTransition (SelectReceiptItem i) SelectingReceiptItems{items, ..} =
    SelectingReceiptItems
      { items = items
          & traversed % unsafeFiltered ((i ==) . (^. _2 % #index)) % _1 %~ not
      , ..}

instance HandleTransitionM StartSelectingRequestRecipient SelectingReceiptItems SelectingRequestRecipient AppM where
  handleTransitionM _ SelectingReceiptItems{..} =
    case nonEmpty (items & filter fst & map snd) of
      Nothing -> do
        replyText "No receipt items selected. Cannot proceed."
        throwError CancelTransition

      Just filteredItems -> do
        return $ SelectingRequestRecipientD{items=filteredItems, ..}

instance HandleTransitionM SelectRequestRecipient SelectingRequestRecipient InitialState AppM where
  handleTransitionM (SelectRequestRecipient recipientId) SelectingRequestRecipientD{qr, items} = do
    let indices = items <&> view #index
    token <- authViaTelegram =<< currentUser
    let reqBody = SendReceiptItemsRequestReqBody
          { receiptQr = qr
          , indexSelections = singleton IndexSelectionReqBody{..}
          }
    runReq_ $ sendRequest token reqBody
    return InitialState -- TODO would be better with intermediate ack state

----------------------------------------------------

addContact :: AddContact -> AppM InitialState
addContact (AddContact usernameType content) = do
    token <- authViaTelegram =<< currentUser
    processContact token
    return InitialState
    where
    processContact token = do
      let (baseUsername, mContactNamePart) = T.span (/= ' ') content
      contactNameResult <- case T.strip mContactNamePart of
        "" -> pure $ Right Nothing
        name -> case mkTextMaxLen name of
          Just validName -> pure $ Right $ Just validName
          Nothing -> do
            replyText $ "contact name " <> name <> " is too long, 50 characters is the max length"
            return $ Left ()

      case contactNameResult of
        Left _ -> pure ()
        Right contactName -> do
          let (query, formattedUsername) = case usernameType of
                Telegram -> (UserTgUsernameQuery baseUsername, "@" <> baseUsername)
                Regular  -> (UserUsernameQuery baseUsername, baseUsername)
          u <- runReq $ getUser token query
          if| Just AuthServiceUser{userId} <- matchUnion @AuthServiceUser u ->
              handleSuccess userId formattedUsername contactName
            | Just _ <- matchUnion @(WithStatus 404 ()) u ->
              handleUserNotFound formattedUsername usernameType
            | otherwise -> return ()
      where
        handleSuccess contactUserId formattedUsername contactName = do
          runReq_ $ createContact token CreateContactReqBody{..}
          replyText $ case contactName of
            Just (TextMaxLen name) ->
              "contact " <> formattedUsername <> " successfully added as " <> name
            Nothing ->
              "contact " <> formattedUsername <> " successfully added"

        handleUserNotFound formattedUsername Telegram = do
          replyText $ T.unlines
            [ "User " <> formattedUsername <> " is not registered in check-check"
            , "Send them the following link to join:"
            ]
          replyText "https://t.me/CheckCheckTgBot?start=start" -- TODO remove hardlink

        handleUserNotFound formattedUsername Regular =
          replyText $ "User " <> formattedUsername <> " is not registered in check-check"
