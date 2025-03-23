{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Clients.Utils
  ( runReq, runReq_
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

runReq :: forall clientM key m env err a.
  ( AsKeyedClientM clientM key
  , MonadIO m, MonadReader env m, HasKeyedClientEnv env key
  , MonadError err m, FromClientError err
  ) => clientM a -> m a
runReq req = do
  clientEnv <- asks $ getClientEnv $ Proxy @key
  result <- liftIO $ runClientM (asClientM req) clientEnv
  liftEither $ mapLeft fromClientError result
  where
    mapLeft f (Left a) = Left $ f a
    mapLeft _ (Right r) = Right r

runReq_ ::
  ( AsKeyedClientM clientM clientKey
  , MonadIO m, MonadReader env m, HasKeyedClientEnv env clientKey
  , MonadError err m, FromClientError err
  ) => clientM a -> m ()
runReq_ = void . runReq
