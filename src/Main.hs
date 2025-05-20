{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Telegram.Bot.API (updateChatId)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Telegram.Bot.API as TG
import Servant.Client (mkClientEnv, BaseUrl, parseBaseUrl)
import System.Environment (getEnv)
import Telegram.Bot.AppM (AppEnv(..), AppM (..))
import Telegram.Bot.FSA
import Telegram.Bot.FSAfe (hoistStartKeyedBot_, getEnvToken, BotM)
import Control.Monad.Reader (ReaderT(..), MonadIO (..))
import Optics ((&))
import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT)
import Data.Proxy (Proxy (..))

data RunConfig = RunConfig
  { authApiKey :: String
  , tgToken :: TG.Token
  , beClientBaseUrl :: BaseUrl
  , authClientBaseUrl :: BaseUrl
  }

type FSA =
 '[ '(InitialState, '[Start, ShowReceipt, AddContact])
  , '(ViewingReceipt, '[SelectReceiptItem, Cancel])
  , '(SelectingReceiptItems, '[SelectReceiptItem, StartSelectingRequestRecipient, Cancel])
  , '(SelectingRequestRecipient, '[SelectRequestRecipient, Cancel])
  ]

run :: RunConfig -> IO ()
run RunConfig{..} = do
  manager <- newManager defaultManagerSettings
  let beClientEnv = mkClientEnv manager beClientBaseUrl
  let authClientEnv = mkClientEnv manager authClientBaseUrl
  let appEnv = AppEnv beClientEnv authClientEnv authApiKey

  hoistStartKeyedBot_ fsa (nt appEnv) updateChatId InitialState tgToken
  where
    fsa = Proxy :: Proxy FSA
    nt :: AppEnv -> AppM a -> BotM a
    nt appEnv appM = unAppM appM `runReaderT` appEnv `evalStateT` Nothing & runExceptT >>= \case
      Right a -> return a
      Left err -> do liftIO $ print err
                     fail ""

main :: IO ()
main = do
  putStrLn "The bot is running"
  authApiKey <- getEnv "AUTH_API_KEY"
  tgToken <- getEnvToken "TELEGRAM_BOT_TOKEN"
  beClientBaseUrl <- parseBaseUrl =<< getEnv "BACKEND_BASE_URL"
  authClientBaseUrl <- parseBaseUrl =<< getEnv "AUTH_BASE_URL"
  run RunConfig{..}

