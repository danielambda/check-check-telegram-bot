{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Telegram.Bot.FSA.Transitions.SelectReceiptItem (handleTransition) where

import Telegram.Bot.Simple (actionButton, editUpdateMessage)
import Optics ((&), (%), unsafeFiltered, traversed, (^.), _1, _2, (%~))

import Telegram.Bot.AppM ((<#), tg, Eff')
import Telegram.Bot.UI (toSelectReceiptItemButton, messageWithButtons, tshow)
import Telegram.Bot.FSA
  ( State(SelectingReceiptItems, ViewingReceipt)
  , Transition (StartSelectingRequestRecipient, CancelSelecingReceiptItems)
  )

handleTransition :: Int -> State -> Eff' Transition State
handleTransition i (ViewingReceipt qr items) =
  handleTransition i (SelectingReceiptItems qr $ (False,) <$> items)

handleTransition i (SelectingReceiptItems qr initialItems) = SelectingReceiptItems qr items <# do
  let itemsButtons = (:[]) . toSelectReceiptItemButton . snd <$> items
  let confirmButton = actionButton "Confirm" StartSelectingRequestRecipient
  let cancelButton  = actionButton "Cancel"  CancelSelecingReceiptItems
  let bottomButtonRow = [confirmButton, cancelButton]
  let buttons = itemsButtons ++ [bottomButtonRow]
  let selectedIndices = (^. _2 % #index) <$> filter fst items
  let editMsg = messageWithButtons ("Selected receipt items: " <> tshow selectedIndices) buttons
  tg $ editUpdateMessage editMsg
  where items = initialItems & traversed % unsafeFiltered ((i ==) . (^. _2 % #index)) % _1 %~ not

handleTransition _ _ = error "TODO"
