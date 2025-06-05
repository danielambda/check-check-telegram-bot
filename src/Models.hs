{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Models
  ( FromResp(..)
  , ToReqBody(..)
  , Request(..), RequestItem(..)
  , Budget(..)
  , requestTotal
  ) where

import Data.UUID (UUID)
import Data.Text (Text)

import Data.List.NonEmpty (NonEmpty)

import Data.Time (UTCTime)
import CheckCheck.Contracts.Budget (BudgetResp(..))
import CheckCheck.Contracts.Requests (RequestResp(..), RequestItemResp(..), RequestItemReqBody(..))

class FromResp resp a | resp -> a, a -> resp where
  fromResp :: resp -> a

class ToReqBody a reqBody | a -> reqBody, reqBody -> a where
  toReqBody :: a -> reqBody

data Request = Request
  { requestId :: UUID
  , isPending :: Bool
  , items :: NonEmpty RequestItem
  , createdAt :: UTCTime
  } deriving (Read, Show)
instance Eq Request where Request{requestId=a} == Request{requestId=b} = a == b

data RequestItem = RequestItem
  { name :: Text
  , price :: Integer
  } deriving (Read, Show)
instance Eq RequestItem where RequestItem{name=a} == RequestItem{name=b} = a == b

instance ToReqBody RequestItem RequestItemReqBody where
  toReqBody RequestItem{..} = RequestItemReqBody{..}

requestTotal :: Request -> Integer
requestTotal = sum . fmap (.price) . (.items)

instance FromResp RequestResp Request where
  fromResp RequestResp{items=itemsResp, ..} = Request{..}
    where items = fmap fromResp itemsResp

instance FromResp RequestItemResp RequestItem where
  fromResp RequestItemResp{price = price, ..} = RequestItem{..}

newtype Budget = Budget
  { amount :: Integer }

instance FromResp BudgetResp Budget where
  fromResp BudgetResp{..} = Budget{..}
