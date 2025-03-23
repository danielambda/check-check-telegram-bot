{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Telegram.Bot.API
  ( BotName(..), Update, defaultTelegramClientEnv , userUsername
  , responseResult
  , updateChatId
  )
import Telegram.Bot.Simple
  ( BotApp(..), Eff, startBot_, getEnvToken
  , conversationBot
  )
import Control.Applicative ((<|>))
import Control.Monad.Reader ( ReaderT (..))
import Control.Monad (unless)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Orphan ()
import qualified Telegram.Bot.API as TG
import Servant.Client (runClientM, mkClientEnv, BaseUrl, ClientEnv, parseBaseUrl)
import System.Environment (getEnv)
import Telegram.Bot.Simple.UpdateParser (parseUpdate, command, commandWithBotName, callbackQueryDataRead)
import Telegram.Bot.AppM (AppEnv(..), Eff')
import Telegram.Bot.FSA (State(..), Transition(..))
import qualified Telegram.Bot.FSA.Transitions.AddContact as AddContact
import qualified Telegram.Bot.FSA.Transitions.CancelSelectingRequestRecipient as CancelSelectingRequestRecipient
import qualified Telegram.Bot.FSA.Transitions.Id as Id
import qualified Telegram.Bot.FSA.Transitions.SelectReceiptItem as SelectReceiptItem
import qualified Telegram.Bot.FSA.Transitions.SelectRequestRecipient as SelectRequestRecipient
import qualified Telegram.Bot.FSA.Transitions.ShowReceipt as ShowReceipt
import qualified Telegram.Bot.FSA.Transitions.Start as Start
import qualified Telegram.Bot.FSA.Transitions.StartSelectingRequestRecipient as StartSelectingRequestRecipient
import qualified Telegram.Bot.FSA.Transitions.CancelViewingReceipt as CancelViewingReceipt
import qualified Telegram.Bot.FSA.Transitions.CancelSelectingReceiptItems as CancelSelecintReceiptItems

mkBotApp :: ClientEnv -> ClientEnv -> String -> BotName -> BotApp State Transition
mkBotApp backendClientEnv authClientEnv secret botName = BotApp
  { botInitialModel = InitialState
  , botAction = flip $ decideTransition botName
  , botHandler = botHandler
  , botJobs = []
  } where
    botHandler = fmap nt . handleTransition
    nt :: Eff' transition state -> Eff transition state
    nt = flip runReaderT AppEnv{..}

decideTransition :: BotName -> State -> Update -> Maybe Transition
decideTransition (BotName botName) state = parseUpdate parser
  where
    command' cmd = command cmd <|> commandWithBotName botName cmd

    parser = case state of
      InitialState
         -> ShowReceipt <$> (command' "qr" <|> command' "receipt")
        <|> AddContact <$> command' "contact"
        <|> Start <$ command' "start"

      ViewingReceipt{} -> callbackQueryDataWhich isAllowed where
        isAllowed CancelViewingReceipt{} = True
        isAllowed SelectReceiptItem{} = True
        isAllowed _ = False

      SelectingReceiptItems{} -> callbackQueryDataWhich isAllowed where
        isAllowed SelectReceiptItem{} = True
        isAllowed CancelSelecingReceiptItems{} = True
        isAllowed StartSelectingRequestRecipient{} = True
        isAllowed _ = False

      SelectingRequestRecipient{} -> callbackQueryDataWhich isAllowed where
        isAllowed SelectRequestRecipient{} = True
        isAllowed CancelSelectingRequestRecipient{} = True
        isAllowed _ = False
      where
        callbackQueryDataWhich isAllowed = do
          transition <- callbackQueryDataRead
          unless (isAllowed transition) $
            fail "unsupported transition"
          return transition

handleTransition :: Transition -> State -> Eff' Transition State
handleTransition Id = Id.handleTransition
handleTransition Start = Start.handleTransition
handleTransition (AddContact contact) = AddContact.handleTransition contact
handleTransition CancelViewingReceipt = CancelViewingReceipt.handleTransition
handleTransition (ShowReceipt qr) = ShowReceipt.handleTransition qr
handleTransition (ShowReceipt' qr items) = ShowReceipt.handleTransition' qr items
handleTransition (SelectReceiptItem i) = SelectReceiptItem.handleTransition i
handleTransition CancelSelecingReceiptItems = CancelSelecintReceiptItems.handleTransition
handleTransition StartSelectingRequestRecipient = StartSelectingRequestRecipient.handleTransition
handleTransition CancelSelectingRequestRecipient = CancelSelectingRequestRecipient.handleTransition
handleTransition (SelectRequestRecipient recipientId) = SelectRequestRecipient.handleTransition recipientId

data RunConfig = RunConfig
  { authApiKey :: String
  , tgToken :: TG.Token
  , beClientBaseUrl :: BaseUrl
  , authClientBaseUrl :: BaseUrl
  }

run :: RunConfig -> IO ()
run RunConfig{..} = do
  tgEnv <- defaultTelegramClientEnv tgToken
  mBotName <- either (error . show) (userUsername . responseResult) <$> runClientM TG.getMe tgEnv
  let botName = maybe (error "bot name is not defined") BotName mBotName

  manager <- newManager defaultManagerSettings
  let beClientEnv = mkClientEnv manager beClientBaseUrl
  let authClientEnv = mkClientEnv manager authClientBaseUrl

  let botApp = conversationBot updateChatId $ mkBotApp beClientEnv authClientEnv authApiKey botName
  startBot_ botApp tgEnv

main :: IO ()
main = do
  putStrLn "The bot is running"
  authApiKey <- getEnv "AUTH_API_KEY"
  tgToken <- getEnvToken "TELEGRAM_BOT_TOKEN"
  beClientBaseUrl <- parseBaseUrl =<< getEnv "BACKEND_BASE_URL"
  authClientBaseUrl <- parseBaseUrl =<< getEnv "AUTH_BASE_URL"
  run RunConfig{..}

