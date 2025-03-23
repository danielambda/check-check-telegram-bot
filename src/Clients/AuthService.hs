{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Clients.AuthService
  ( authTelegram, getUser
  , AuthServiceUser(..), UserQuery(..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Proxy (Proxy(..))
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Servant.API
  ( JSON, (:>), Post, Header', Required, Strict, ReqBody, Capture
  , ToHttpApiData (toUrlPiece), UVerb, StdMethod (GET)
  , Union, HasStatus(StatusOf), WithStatus
  )
import Servant.Auth (Auth, JWT)
import Servant.Auth.Client (Token (Token))
import Servant.Client (client, ClientM, hoistClient)
import Telegram.Bot.API (UserId (..), User (..))

import GHC.Generics (Generic)

import CheckCheck.Contracts.Users (AuthenticatedUser)
import Clients.Utils (AsKeyedClientM (..))

getUser :: Token -> UserQuery -> AuthServiceClientM (Union '[AuthServiceUser, WithStatus 404 ()])
getUser = hoistClient endpoint nt $ client endpoint
  where
    endpoint = Proxy :: Proxy GetUser
    nt = AuthServiceClientM

authTelegram :: String -> User -> AuthServiceClientM (Token, UTCTime)
authTelegram apiKey User{ userId = UserId tgUserId, userUsername = Just tgUsername } = do
  let authData = TgAuthData{..}
  authResultToToken <$> authTelegram' authData apiKey
  where
    authResultToToken TgAuthResult{ expirationTime, token } =
      (Token $ encodeUtf8 token, expirationTime)

    authTelegram' = hoistClient endpoint nt $ client endpoint
      where
        endpoint = Proxy :: Proxy AuthTelegram
        nt = AuthServiceClientM
authTelegram _ _ = error "user does not have a username"

data UserQuery
  = UserUserIdQuery UUID
  | UserUsernameQuery Text
  | UserTgUserIdQuery Int
  | UserTgUsernameQuery Text

instance ToHttpApiData UserQuery where
  toUrlPiece (UserUserIdQuery userId) = "user-id:" <> UUID.toText userId
  toUrlPiece (UserUsernameQuery username) = "username:" <> username
  toUrlPiece (UserTgUserIdQuery tgUserId) = "tg-user-id:" <> T.pack (show tgUserId)
  toUrlPiece (UserTgUsernameQuery tgUsername) = "tg-username:" <> tgUsername

data AuthServiceUser = AuthServiceUser
  { userId :: UUID
  , username :: Text
  , tgUserId :: Maybe Int
  , tgUsername :: Maybe Text
  } deriving (Generic, FromJSON)

instance HasStatus AuthServiceUser where
  type StatusOf AuthServiceUser = 200

data TgAuthData = TgAuthData
  { tgUserId :: Integer
  , tgUsername :: Text
  } deriving (Generic, ToJSON)

data TgAuthResult = TgAuthResult
  { token :: Text
  , expirationTime :: UTCTime
  } deriving (Generic, FromJSON)

type Header = Header' [Required, Strict]
type AuthTelegram = "auth" :> "telegram" :>
  ReqBody '[JSON] TgAuthData :> Header "x-api-key" String :> Post '[JSON] TgAuthResult

type GetUser = "users" :> Auth '[JWT] AuthenticatedUser
  :> Capture "query" UserQuery :> UVerb 'GET '[JSON] '[AuthServiceUser, WithStatus 404 ()]

newtype AuthServiceClientM a = AuthServiceClientM { unAuthServiceClientM :: ClientM a }
  deriving newtype (Functor, Applicative, Monad)

instance AsKeyedClientM AuthServiceClientM "auth" where
  asClientM = unAuthServiceClientM
