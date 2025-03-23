{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Telegram.Bot.AppM
  ( AppM(..), AppError(..), AppEnv(..), tg
  , Eff', (<#)
  , authViaTelegram, currentUser
  ) where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (..), MonadReader (ask), asks)
import Control.Monad.State (StateT (..), MonadState (get, put))
import Control.Monad.Trans (lift)
import Data.Time (UTCTime, addUTCTime, secondsToNominalDiffTime, getCurrentTime)
import Servant.Auth.Client (Token)
import Servant.Client (ClientError, ClientEnv)
import Telegram.Bot.Simple (BotM, GetAction, Eff, withEffect, BotContext (botContextUpdate))

import Data.Function ((&))

import Clients.Utils (FromClientError (..), HasKeyedClientEnv (..), runReq)
import Telegram.Bot.FSA (Transition, State)
import Orphan ()
import qualified Telegram.Bot.API as TG
import Clients.AuthService (authTelegram)
import Control.Monad ((>=>))
import Control.Applicative ((<|>))

newtype AppM a = AppM
  { unAppM :: ReaderT AppEnv (StateT (Maybe (Token, UTCTime)) (ExceptT AppError BotM)) a }
  deriving
    ( Functor, Applicative
    , Monad, MonadIO
    , MonadReader AppEnv
    , MonadState (Maybe (Token, UTCTime))
    , MonadError AppError
    )

newtype AppError = AppClientError ClientError
  deriving (Show)

instance FromClientError AppError where
  fromClientError = AppClientError

data AppEnv = AppEnv
  { backendClientEnv :: ClientEnv
  , authClientEnv :: ClientEnv
  , secret :: String
  }

instance HasKeyedClientEnv AppEnv "backend" where
  getClientEnv _ = backendClientEnv

instance HasKeyedClientEnv AppEnv "auth" where
  getClientEnv _ = authClientEnv

tg :: BotM a -> AppM a
tg = AppM . lift . lift . lift

authViaTelegram :: TG.User -> AppM Token
authViaTelegram user = do
  afterFiveMinutes <- liftIO $ addUTCTime (secondsToNominalDiffTime 300) <$> getCurrentTime
  get >>= \case
    Just (token, expirationTime) | afterFiveMinutes < expirationTime ->
      return token
    _ -> do
      secret' <- asks secret
      (token, expirationTime) <- runReq $ authTelegram secret' user
      put $ Just (token, expirationTime)
      return token

-- TODO remove this function and replace it with users parsed alongside with transitions
currentUser :: AppM TG.User
currentUser = AppM $ lift $ do
  userFromMessage <- asks $ botContextUpdate >=> TG.updateMessage >=> TG.messageFrom
  userFromCallback <- asks $ botContextUpdate >=> TG.updateCallbackQuery .> fmap TG.callbackQueryFrom
  maybe (throwError (AppClientError undefined)) return $ userFromMessage <|> userFromCallback -- TODO add proper error to throw
  where (.>) = flip (.)


type Eff' action = ReaderT AppEnv (Eff action)

infix 0 <#
(<#) :: GetAction a Transition => State -> AppM a -> Eff' Transition State
state <# AppM app = do
  env <- ask
  let bot = app `runReaderT` env `runStateT` Nothing & runExceptT >>= \case
        Right (a, _) -> return $ Just a
        Left err -> do liftIO $ print err
                       return Nothing
  lift $ withEffect bot state

