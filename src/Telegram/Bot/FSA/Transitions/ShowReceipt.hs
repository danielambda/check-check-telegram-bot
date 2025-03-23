{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.FSA.Transitions.ShowReceipt (handleTransition, handleTransition') where

import Telegram.Bot.Simple (reply, actionButton)
import Data.Text (Text)

import CheckCheck.Contracts.Receipts (ReceiptResp(ReceiptResp))
import Clients.Utils (runReq)
import Clients.Backend (getReceipt)
import Models (FromResp (fromResp), ReceiptItem(..))
import Telegram.Bot.AppM ((<#), Eff', tg)
import Telegram.Bot.UI (messageWithButtons, toSelectReceiptItemButton)
import Telegram.Bot.FSA
  ( State(InitialState, ViewingReceipt)
  , Transition (ShowReceipt', CancelViewingReceipt)
  )

handleTransition :: Text -> State -> Eff' Transition State
handleTransition qr InitialState = InitialState <# do
  ReceiptResp respItems <- runReq $ getReceipt qr
  let items = fromResp <$> respItems
  let itemsButtons = (:[]) . toSelectReceiptItemButton <$> items
  let okButton = actionButton "Ok" CancelViewingReceipt
  let buttons = itemsButtons ++ [[okButton]]
  let msg = messageWithButtons "Scanned receipt items: " buttons
  tg $ reply msg
  return $ ShowReceipt' qr items

handleTransition _ _ = error "TODO"

handleTransition' :: Text -> [ReceiptItem] -> State -> Eff' Transition State
handleTransition' qr items InitialState = pure $ ViewingReceipt qr items
handleTransition' _ _ _ = error "TODO"

