{-# LANGUAGE RecordWildCards #-}

module Clients.Backend
  ( postRequest, getRequests, completeReqeust
  , getBudget, applyDeltaToBudget
  , BackendClientM
  ) where

import Data.Proxy (Proxy(..))
import Data.UUID (UUID)
import Servant.API ((:<|>)(..))
import Servant.Client (ClientM, client, hoistClient)

import CheckCheck.Contracts.API (API)
import CheckCheck.Contracts.Requests (PostRequestReqBody, RequestResp)
import CheckCheck.Contracts.Budget (BudgetResp)
import Clients.Utils (AsKeyedClientM (..))

postRequest :: PostRequestReqBody -> BackendClientM RequestResp
postRequest = postRequest' $ requestsClient' apiClient

getRequests :: BackendClientM [RequestResp]
getRequests = getRequests' $ requestsClient' apiClient

completeReqeust :: UUID -> BackendClientM BudgetResp
completeReqeust = completeReqeust' $ requestsClient' apiClient

getBudget :: BackendClientM BudgetResp
getBudget = getBudget' $ budgetClient' apiClient

applyDeltaToBudget :: Integer -> BackendClientM BudgetResp
applyDeltaToBudget = applyDeltaToBudget' $ budgetClient' apiClient

newtype BackendClientM a = BackendClientM { unBackendClientM :: ClientM a }
  deriving (Functor, Applicative, Monad)

instance AsKeyedClientM BackendClientM "backend" where
  asClientM = unBackendClientM

data ApiClient = ApiClient
  { requestsClient' :: ReqeustsClient
  , budgetClient' :: BudgetClient
  }

data ReqeustsClient = ReqeustsClient
  { getRequests' :: BackendClientM [RequestResp]
  , postRequest' :: PostRequestReqBody -> BackendClientM RequestResp
  , completeReqeust' :: UUID -> BackendClientM BudgetResp
  }

data BudgetClient = BudgetClient
  { getBudget' :: BackendClientM BudgetResp
  , applyDeltaToBudget' :: Integer -> BackendClientM BudgetResp
  }

apiClient :: ApiClient
apiClient = ApiClient{..}
  where
    api = Proxy :: Proxy API

    requestsClient :<|> budgetClient = hoistClient api BackendClientM $ client api

    requestsClient' = ReqeustsClient{..}
      where
        postRequest' :<|> getRequests' :<|> completeReqeust' = requestsClient

    budgetClient' = BudgetClient{..}
      where
        getBudget' :<|> applyDeltaToBudget' = budgetClient
