{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Clients.Utils
  ( runReq_, RunReq(..)
  , AsKeyedClientM(..)
  , HasKeyedClientEnv(..)
  , FromClientError(..)
  ) where

import Servant.Client (ClientM, ClientEnv, ClientError, runClientM)

import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, asks)
import Data.Functor (void)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Symbol)

class Monad m => AsKeyedClientM m (key :: Symbol) | m -> key where
  asClientM :: m a -> ClientM a

instance AsKeyedClientM ClientM "" where
  asClientM = id

class HasKeyedClientEnv a (key :: Symbol) where
  getClientEnv :: Proxy key -> a -> ClientEnv

class FromClientError a where
  fromClientError :: ClientError -> a

class MonadIO m => RunReq clientM m where
  runReq :: clientM a -> m a

instance
  ( AsKeyedClientM clientM key
  , MonadIO m, MonadReader env m, HasKeyedClientEnv env key
  , MonadError err m, FromClientError err
  ) => RunReq clientM m where
  runReq req = do
    clientEnv <- asks $ getClientEnv $ Proxy @key
    result <- liftIO $ runClientM (asClientM req) clientEnv
    liftEither $ mapLeft fromClientError result
    where
      mapLeft f (Left a) = Left $ f a
      mapLeft _ (Right r) = Right r

runReq_ :: RunReq clientM m => clientM a -> m ()
runReq_ = void . runReq
