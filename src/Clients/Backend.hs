{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Clients.Backend
  ( getReceipt
  , getMe
  , createContact, getContacts, deleteContact
  , sendRequest
  ) where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.UUID (UUID)
import Servant.API ((:<|>)(..), NoContent)
import Servant.Auth.Client (Token)
import Servant.Client (ClientM, client, hoistClient)

import CheckCheck.Contracts.API (API)
import CheckCheck.Contracts.Groups (CreateGroupReqBody, GroupResp)
import CheckCheck.Contracts.Receipts (ReceiptResp)
import CheckCheck.Contracts.Users (UserResp)
import CheckCheck.Contracts.Users.OutgoingRequests (SendRequestReqBody, RequestResp)
import CheckCheck.Contracts.Users.Contacts (CreateContactReqBody, ContactResp)
import Clients.Utils (AsKeyedClientM (..))

getReceipt :: Text -> BackendClientM ReceiptResp
getReceipt = getReceipt' apiClient

getMe :: Token -> BackendClientM UserResp
getMe token = getMe' $ mkUsersClient apiClient token

createContact :: Token -> CreateContactReqBody -> BackendClientM NoContent
createContact = createContact' . contactsClient . mkUsersClient apiClient

getContacts :: Token -> BackendClientM [ContactResp]
getContacts = getContacts' . contactsClient . mkUsersClient apiClient

deleteContact :: Token -> UUID -> BackendClientM NoContent
deleteContact = deleteContact' . contactsClient . mkUsersClient apiClient

sendRequest :: Token -> SendRequestReqBody -> BackendClientM [RequestResp]
sendRequest = sendRequest' . outgoingRequestsClient . mkUsersClient apiClient

newtype BackendClientM a = BackendClientM { unBackendClientM :: ClientM a }
  deriving (Functor, Applicative, Monad)

instance AsKeyedClientM BackendClientM "backend" where
  asClientM = unBackendClientM

data ApiClient = ApiClient
  { getReceipt' :: Text -> BackendClientM ReceiptResp
  , mkGroupsClient :: Token -> GroupsClient
  , mkUsersClient :: Token -> UsersClient
  }

data GroupsClient = GroupsClient
  { createGroup' :: CreateGroupReqBody -> BackendClientM GroupResp
  , getGroup' :: UUID -> BackendClientM GroupResp
  , getAllGroups' :: BackendClientM [GroupResp]
  }

data UsersClient = UsersClient
  { getMe' :: BackendClientM UserResp
  , contactsClient :: ContactsClient
  , outgoingRequestsClient :: OutgoingRequestsClient
  }

data ContactsClient = ContactsClient
  { createContact' :: CreateContactReqBody -> BackendClientM NoContent
  , getContacts' :: BackendClientM [ContactResp]
  , deleteContact' :: UUID -> BackendClientM NoContent
  }

newtype OutgoingRequestsClient = OutgoingRequestsClient
  { sendRequest' :: SendRequestReqBody -> BackendClientM [RequestResp] }

apiClient :: ApiClient
apiClient = ApiClient{..}
  where
    api = Proxy :: Proxy API

    getReceipt' :<|> groupsClient :<|> usersClient = hoistClient api BackendClientM $ client api

    mkUsersClient token = UsersClient{..}
      where
        (getMe' :<|> _) :<|> contactsClient' :<|> outgoingRequestsClient' :<|> _ = usersClient token

        contactsClient = ContactsClient{..}
          where
            getContacts' :<|> createContact' :<|> deleteContact' = contactsClient'

        outgoingRequestsClient = OutgoingRequestsClient{..}
          where
            sendRequest' = outgoingRequestsClient'

    mkGroupsClient token = GroupsClient{..}
      where
        createGroup' :<|> getGroup' :<|> getAllGroups' :<|> _ = groupsClient token

