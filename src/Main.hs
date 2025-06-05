{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Telegram.Bot.API (updateChatId, ParseMode (..), SomeReplyMarkup (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Data.Proxy
import qualified Telegram.Bot.API as TG
import Servant.Client
import System.Environment (getEnv)
import Telegram.Bot.AppM (AppM (..), AppEnv (..))
import Control.Monad.Reader (ReaderT(..))

import Telegram.Bot.FSAfe
import Data.Text (Text)
import Models
import Clients.Backend
import Clients.Utils
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Generics (Generic)
import Control.Monad.Except (runExceptT)
import Data.List ((!?), unsnoc)
import Control.Monad.IO.Class (liftIO)
import Data.Time (showGregorian, UTCTime (utctDay))
import Data.Function ((&))
import CheckCheck.Contracts.Budget (BudgetResp(BudgetResp))
import Text.Read (readMaybe)
import CheckCheck.Contracts.Requests (PostRequestReqBody(PostRequestReqBody))
import Data.List.NonEmpty (NonEmpty, nonEmpty)

data InitialState = InitialState
instance StateMessageM AppM InitialState where
  stateMessageM _ = Send <$> do
    BudgetResp budget <- runReq getBudget
    return $
      textMessage (T.unlines
      [ "*Your budget is*"
      , tshow budget
      ])
      & withParseMode MarkdownV2

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
    $ (if isPending req then "Request is pending 👻" else "Request is done ✅")
    : map formatRequestItem (NonEmpty.toList $ items req))
    & withInlineKeyboard
      [ row [button "Pay" CompleteRequest | isPending req]
      , single $ button "Back" Cancel
      ]
    where
      formatRequestItem RequestItem{name, price} =
        name <> " : " <> tshow price <> " rub"

data Start = Start
  deriving (Generic, IsUnit)
  deriving ParseTransition via CommandUnit "start" Start
instance HandleTransitionM Start InitialState InitialState AppM where
  handleTransitionM Start InitialState = do
    sendText_ "Nice to see you"
    return InitialState

newtype PostRequest = PostRequest (NonEmpty RequestItem)
instance ParseTransition PostRequest where
  parseTransition = do
    txt <- plainText
    case nonEmpty $ T.lines txt of
      Nothing -> fail "empty message"
      Just lns -> PostRequest <$> traverse f lns
    where
      f arg = case T.words arg of
        [] -> fail "empty line"
        hd : tl -> case readMaybe (T.unpack hd) of
          Just num -> pure $ RequestItem (T.unwords tl) num
          Nothing -> maybe (fail "no price") pure $ do
            (nameWords, priceStr) <- unsnoc (hd : tl)
            price <- readMaybe (T.unpack priceStr)
            return $ RequestItem (T.unwords nameWords) price
instance HandleTransitionM PostRequest InitialState InitialState AppM where
  handleTransitionM (PostRequest reqItems) InitialState = do
    let reqItemsReqBody = fmap toReqBody reqItems
    runReq_ $ postRequest $ PostRequestReqBody reqItemsReqBody
    return InitialState

data ShowRequests = ShowRequests
  deriving (Generic, IsUnit)
  deriving ParseTransition via CommandUnit "requests" ShowRequests
instance HandleTransitionM ShowRequests InitialState ViewingRequests AppM where
  handleTransitionM ShowRequests InitialState = do
    requests <- map fromResp <$> runReq getRequests
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
  handleTransition (ShowRequest req) = ViewingRequest req

data CompleteRequest = CompleteRequest
  deriving (Read, Show)
  deriving IsCallbackQuery via ReadShow CompleteRequest
  deriving ParseTransition via CallbackQueryData CompleteRequest
instance HandleTransitionM CompleteRequest ViewingRequest InitialState AppM where
  handleTransitionM CompleteRequest (ViewingRequest req _) = do
    runReq_ $ completeReqeust req.requestId
    return InitialState

newtype ApplyBudgetDelta = ApplyBudgetDelta Integer
instance ParseTransition ApplyBudgetDelta where
  parseTransition = do
    arg <- command "budget"
    let mDelta = case T.uncons arg of
          Just ('+', deltaStr) -> readMaybe $ T.unpack deltaStr
          Just ('-', deltaStr) -> fmap negate $ readMaybe $ T.unpack deltaStr
          Just (hd, tl) -> readMaybe $ T.unpack $ T.cons hd tl
          _ -> Nothing
    delta <- case mDelta of
      Just a -> pure a
      Nothing -> fail "no delta"
    return $ ApplyBudgetDelta delta
instance HandleTransitionM ApplyBudgetDelta InitialState InitialState AppM where
  handleTransitionM (ApplyBudgetDelta delta) InitialState = do
    runReq_ $ applyDeltaToBudget delta
    return InitialState

data Cancel = Cancel
  deriving (Read, Show)
  deriving IsCallbackQuery via ReadShow Cancel
  deriving ParseTransition via CallbackQueryData Cancel

instance MonadBot m => HandleTransitionM Cancel ViewingRequests InitialState m where
  handleTransitionM Cancel ViewingRequests{} = do
    editUpdateMessageReplyMarkup_ $ SomeInlineKeyboardMarkup $ col []
    return InitialState

instance HandleTransition Cancel ViewingRequest ViewingRequests where
  handleTransition Cancel (ViewingRequest _ state) = state

data RunConfig = RunConfig
  { tgToken :: TG.Token
  , beClientBaseUrl :: BaseUrl
  }

type FSA =
 '[ '(InitialState, '[Start, PostRequest, ShowRequests, ApplyBudgetDelta])
  , '(ViewingRequests, '[ShowRequest, Cancel])
  , '(ViewingRequest, '[CompleteRequest, Cancel])
  ]

run :: RunConfig -> IO ()
run RunConfig{..} = do
  manager <- newManager defaultManagerSettings
  let backendClientEnv = mkClientEnv manager   beClientBaseUrl

  hoistStartKeyedBot_ (Proxy @FSA) (nt AppEnv{..}) updateChatId InitialState tgToken
  where nt env (AppM app) = app `runReaderT` env & runExceptT >>= \case
          Right a -> pure a
          Left err -> do
            liftIO $ print err
            fail $ show err

main :: IO ()
main = do
  putStrLn "The bot is running"
  tgToken <- getEnvToken "TELEGRAM_BOT_TOKEN"
  beClientBaseUrl <- parseBaseUrl =<< getEnv "BACKEND_BASE_URL"
  run RunConfig{..}

tshow :: Show a => a -> T.Text
tshow = T.pack . show

formatRequest :: Request -> Text
formatRequest req
  =   T.pack (showGregorian req.createdAt.utctDay)
  <> ": "
  <> tshow (requestTotal req)
  <> " rub"
  <> if req.isPending then "" else " ✅"

