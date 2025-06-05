{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Telegram.Bot.AppM
  ( AppM(..), AppError(..), AppEnv(..)
  , currentUser
  ) where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (..), MonadReader, asks)
import Control.Monad.Trans (lift)
import Servant.Client (ClientError, ClientEnv)

import Clients.Utils (FromClientError (..), HasKeyedClientEnv (..))
import qualified Telegram.Bot.API as TG
import Control.Monad ((>=>))
import Control.Applicative ((<|>))
import Telegram.Bot.FSAfe

newtype AppM a = AppM
  { unAppM :: ReaderT AppEnv (ExceptT AppError BotM) a }
  deriving
    ( Functor, Applicative
    , Monad, MonadIO
    , MonadReader AppEnv
    , MonadError AppError
    )

newtype AppError = AppClientError ClientError
  deriving (Show)

instance FromClientError AppError where
  fromClientError = AppClientError

newtype AppEnv = AppEnv
  { backendClientEnv :: ClientEnv }

instance HasKeyedClientEnv AppEnv "backend" where
  getClientEnv _ = backendClientEnv

instance MonadBot AppM where
  liftBot = AppM . lift . lift

-- TODO remove this function and replace it with users parsed alongside with transitions
currentUser :: AppM TG.User
currentUser = AppM $ lift $ do
  userFromMessage <- asks $ botContextUpdate .> TG.updateMessage >=> TG.messageFrom
  userFromCallback <- asks $ botContextUpdate .> TG.updateCallbackQuery .> fmap TG.callbackQueryFrom
  maybe (throwError (AppClientError undefined)) return $ userFromMessage <|> userFromCallback -- TODO add proper error to throw
  where (.>) = flip (.)

