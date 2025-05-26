{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Clients.Backend
  ( getReceipt
  , getMe
  , createContact, getContacts, deleteContact
  , sendRequest
  , getIncomingRequests, completeIncomingRequest
  , BackendClientM
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
import CheckCheck.Contracts.Users.OutgoingRequests (SendRequestReqBody)
import CheckCheck.Contracts.Users.Contacts (CreateContactReqBody, ContactResp)
import CheckCheck.Contracts.Users.IncomingRequests (CompleteIncomingRequestReqBody, CompleteIncomingRequestResp)
import qualified CheckCheck.Contracts.Users.IncomingRequests as Incoming (RequestResp)
import qualified CheckCheck.Contracts.Users.OutgoingRequests as Outgoing (RequestResp)

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

sendRequest :: Token -> SendRequestReqBody -> BackendClientM [Outgoing.RequestResp]
sendRequest = sendRequest' . outgoingRequestsClient . mkUsersClient apiClient

getIncomingRequests :: Token -> BackendClientM [Incoming.RequestResp]
getIncomingRequests = getIncomingRequests' . incomingRequestsClient . mkUsersClient apiClient

completeIncomingRequest :: Token -> UUID -> CompleteIncomingRequestReqBody -> BackendClientM CompleteIncomingRequestResp
completeIncomingRequest = completeIncomingRequest' . incomingRequestsClient . mkUsersClient apiClient

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
  , incomingRequestsClient :: IncomingRequestsClient
  }

data ContactsClient = ContactsClient
  { createContact' :: CreateContactReqBody -> BackendClientM NoContent
  , getContacts' :: BackendClientM [ContactResp]
  , deleteContact' :: UUID -> BackendClientM NoContent
  }

newtype OutgoingRequestsClient = OutgoingRequestsClient
  { sendRequest' :: SendRequestReqBody -> BackendClientM [Outgoing.RequestResp] }

data IncomingRequestsClient = IncomingRequestsClient
  { getIncomingRequests' :: BackendClientM [Incoming.RequestResp]
  , completeIncomingRequest' :: UUID -> CompleteIncomingRequestReqBody -> BackendClientM CompleteIncomingRequestResp
  }

apiClient :: ApiClient
apiClient = ApiClient{..}
  where
    api = Proxy :: Proxy API

    getReceipt' :<|> groupsClient :<|> usersClient = hoistClient api BackendClientM $ client api

    mkUsersClient token = UsersClient{..}
      where
        (getMe' :<|> _) :<|> contactsClient'
          :<|> outgoingRequestsClient' :<|> incomingRequestsClient' :<|> _ = usersClient token

        contactsClient = ContactsClient{..}
          where
            getContacts' :<|> createContact' :<|> deleteContact' = contactsClient'

        outgoingRequestsClient = OutgoingRequestsClient{..}
          where
            sendRequest' = outgoingRequestsClient'

        incomingRequestsClient = IncomingRequestsClient{..}
          where
            getIncomingRequests' :<|> completeIncomingRequest' = incomingRequestsClient'

    mkGroupsClient token = GroupsClient{..}
      where
        createGroup' :<|> getGroup' :<|> getAllGroups' :<|> _ = groupsClient token

