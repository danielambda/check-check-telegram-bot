{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Telegram.Bot.FSA
  ( State(..)
  , Transition(..)
  ) where

import Data.Text (Text)
import Data.UUID (UUID)

import Data.List.NonEmpty (NonEmpty)

import Models (ReceiptItem(..))

data State
  = InitialState
  | ViewingReceipt Text [ReceiptItem]
  | SelectingReceiptItems Text [(Bool, ReceiptItem)]
  | SelectingRequestRecipient Text (NonEmpty ReceiptItem)
  deriving Show

data Transition
  = Id
  | Start
  | AddContact Text
  | ShowReceipt Text
  | ShowReceipt' Text [ReceiptItem]
  | CancelViewingReceipt
  | SelectReceiptItem Int
  | CancelSelecingReceiptItems
  | StartSelectingRequestRecipient
  | SelectRequestRecipient UUID
  | CancelSelectingRequestRecipient
  deriving (Read, Show)

