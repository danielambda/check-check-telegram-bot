{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Bot.UI
  ( tshow, messageWithButtons
  , formatReceiptItem
  , toSelectReceiptItemButton
  , toSelectRequestRecipientButton
  ) where

import Optics (view)
import Telegram.Bot.Simple
  ( actionButton
  , EditMessage (editMessageReplyMarkup), toEditMessage
  , ReplyMessage (replyMessageReplyMarkup), toReplyMessage
  )
import Telegram.Bot.API
  ( InlineKeyboardButton, InlineKeyboardMarkup (InlineKeyboardMarkup)
  , SomeReplyMarkup (SomeInlineKeyboardMarkup)
  )
import qualified Data.Text as T

import Control.Arrow ((&&&))

import Models (ReceiptItem(..), UserContact (..))
import Telegram.Bot.FSA (Transition (SelectReceiptItem, SelectRequestRecipient))
import SmartPrimitives.TextLenRange (TextLenRange(..))
import SmartPrimitives.TextMaxLen (unTextMaxLen)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

class MessageWithButtons a where
  messageWithButtons :: T.Text -> [[InlineKeyboardButton]] -> a

instance MessageWithButtons ReplyMessage where
  messageWithButtons txt buttons = (toReplyMessage txt)
    {replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $ InlineKeyboardMarkup buttons}

instance MessageWithButtons EditMessage where
  messageWithButtons txt buttons = (toEditMessage txt)
    {editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $ InlineKeyboardMarkup buttons}

formatReceiptItem :: ReceiptItem -> T.Text
formatReceiptItem item@ReceiptItem{index, name, quantity}
  =  tshow index <> ". "
  <> T.take 20 name <> "... x "
  <> tshow quantity <> " = "
  <> tshow priceSum <> " rub"
  where
  priceSum = view #itemTotal item `divide` 100 :: Double
    where divide a b = fromIntegral a / b

toSelectReceiptItemButton :: ReceiptItem -> InlineKeyboardButton
toSelectReceiptItemButton
  = (\(i, item) -> actionButton item (SelectReceiptItem i))
  . process
  where
  process = view #index &&& formatReceiptItem

toSelectRequestRecipientButton :: UserContact -> InlineKeyboardButton
toSelectRequestRecipientButton UserContact{ contactUsername = TextLenRange contactUsername, ..} =
  let name = maybe contactUsername unTextMaxLen mContactName
  in actionButton name (SelectRequestRecipient contactUserId)
