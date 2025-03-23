{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Models (FromResp(fromResp), UserContact(..), ReceiptItem(..)) where

import Data.UUID (UUID)

import SmartPrimitives.TextLenRange (TextLenRange)
import SmartPrimitives.TextMaxLen (TextMaxLen)
import Data.Text (Text)
import Optics (LabelOptic (labelOptic), A_Getter, to)
import CheckCheck.Contracts.Receipts (ReceiptItemResp (..))
import SmartPrimitives.Positive (Positive(..))
import CheckCheck.Contracts.Users.Contacts (ContactResp (..))

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

instance FromResp ReceiptItemResp ReceiptItem where
  fromResp ReceiptItemResp{price = Positive price, quantity = Positive quantity, ..} =
    ReceiptItem{..}

instance LabelOptic "index" A_Getter ReceiptItem ReceiptItem Int Int where
  labelOptic = to $ \ReceiptItem{index} ->  index
instance LabelOptic "name" A_Getter ReceiptItem ReceiptItem Text Text where
  labelOptic = to $ \ReceiptItem{name} -> name
instance LabelOptic "itemTotal" A_Getter ReceiptItem ReceiptItem Integer Integer where
  labelOptic = to $ \ReceiptItem {price, quantity} -> round $ fromInteger price * quantity


