{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Models
  ( FromResp(..)
  , UserContact(..)
  , ReceiptItem(..)
  , Request(..), RequestItem(..)
  , Budget(..)
  ) where

import Data.UUID (UUID)
import Data.Text (Text)
import Optics (LabelOptic(..), A_Getter, to, A_Fold, folding, sumOf, (%))

import Data.List.NonEmpty (NonEmpty)

import SmartPrimitives.Positive (Positive(..))
import SmartPrimitives.TextLenRange (TextLenRange)
import SmartPrimitives.TextMaxLen (TextMaxLen)
import CheckCheck.Contracts.Receipts (ReceiptItemResp (..))
import CheckCheck.Contracts.Users.Contacts (ContactResp (..))
import CheckCheck.Contracts.Users.IncomingRequests (RequestResp (..), RequestItemResp (..))
import Data.Time (UTCTime)
import CheckCheck.Contracts.Users.Budget (BudgetResp (..))

class FromResp resp a | resp -> a where
  fromResp :: resp -> a

data UserContact = UserContact
  { contactUserId :: UUID
  , contactUsername :: TextLenRange 2 50
  , mContactName :: Maybe (TextMaxLen 50)
  }

instance FromResp ContactResp UserContact where
  fromResp ContactResp{..} = UserContact{ mContactName = contactName, ..}


data ReceiptItem = ReceiptItem
  { index :: Int
  , name :: Text
  , price :: Integer
  , quantity :: Double
  } deriving (Show, Read)

instance Eq ReceiptItem where
  ReceiptItem{index=i} == ReceiptItem{index=j} = i == j

instance Ord ReceiptItem where
  ReceiptItem{index=i} <= ReceiptItem{index=j} = i <= j

instance FromResp ReceiptItemResp ReceiptItem where
  fromResp ReceiptItemResp{price = Positive price, quantity = Positive quantity, ..} =
    ReceiptItem{..}

instance LabelOptic "index" A_Getter ReceiptItem ReceiptItem Int Int where
  labelOptic = to $ \ReceiptItem{index} ->  index
instance LabelOptic "name" A_Getter ReceiptItem ReceiptItem Text Text where
  labelOptic = to $ \ReceiptItem{name} -> name
instance LabelOptic "itemTotal" A_Getter ReceiptItem ReceiptItem Integer Integer where
  labelOptic = to $ \ReceiptItem{price, quantity} -> round $ fromInteger price * quantity


data Request = Request
  { requestId :: UUID
  , isPending :: Bool
  , senderId :: UUID
  , items :: NonEmpty RequestItem
  , createdAt :: UTCTime
  } deriving (Read, Show)
instance Eq Request where Request{requestId=a} == Request{requestId=b} = a == b

data RequestItem = RequestItem
  { identity :: Text
  , quantity :: Double
  , price :: Integer
  } deriving (Read, Show)
instance Eq RequestItem where RequestItem{identity=a} == RequestItem{identity=b} = a == b

instance FromResp RequestResp Request where
  fromResp RequestResp{items=itemsResp, ..} = Request{..}
    where items = fmap fromResp itemsResp

instance FromResp RequestItemResp RequestItem where
  fromResp RequestItemResp{price = Positive price, quantity = Positive quantity, ..} =
    RequestItem{..}

instance LabelOptic "items" A_Fold Request Request RequestItem RequestItem where
  labelOptic = folding $ \Request{items} -> items
instance LabelOptic "isPending" A_Getter Request Request Bool Bool where
  labelOptic  = to $ \Request{isPending} -> isPending
instance LabelOptic "createdAt" A_Getter Request Request UTCTime UTCTime where
  labelOptic  = to $ \Request{createdAt} -> createdAt
instance LabelOptic "requestTotal" A_Getter Request Request Integer Integer where
  labelOptic = to $ sumOf $ #items % #itemTotal
instance LabelOptic "itemTotal" A_Getter RequestItem RequestItem Integer Integer where
  labelOptic = to $ \RequestItem{quantity, price} -> round $ fromInteger price * quantity

data Budget = Budget
  { isLowerBoundExceeded :: Bool
  , lowerBound :: Maybe Integer
  , amount :: Integer
  }

instance FromResp BudgetResp Budget where
  fromResp BudgetResp{..} = Budget{..}
