{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.FSA.Transitions.CancelViewingReceipt (handleTransition) where

import Telegram.Bot.FSA (State (ViewingReceipt, InitialState), Transition)
import Telegram.Bot.AppM ((<#), Eff', tg)
import Telegram.Bot.UI (formatReceiptItem)
import qualified Data.Text as T
import Telegram.Bot.Simple (editUpdateMessageText)

handleTransition :: State -> Eff' Transition State
handleTransition (ViewingReceipt _ items) = InitialState <# do
  let msgTxt = T.unlines $ "Scanned receipt items: " : map formatReceiptItem items
  tg $ editUpdateMessageText msgTxt

handleTransition _ = error "TODO"
