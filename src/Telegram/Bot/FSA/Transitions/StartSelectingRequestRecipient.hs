{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Telegram.Bot.FSA.Transitions.StartSelectingRequestRecipient (handleTransition) where

import Optics ((&), view, (<&>))
import qualified Data.Text as T
import Telegram.Bot.Simple (replyText, reply, actionButton, editUpdateMessageText)

import Data.Foldable (toList)
import Data.List.NonEmpty (nonEmpty)
import GHC.Float (divideDouble)

import Clients.Backend (getContacts)
import Clients.Utils (runReq)
import Models (FromResp (fromResp))
import Telegram.Bot.AppM ((<#), tg, authViaTelegram, currentUser, Eff')
import Telegram.Bot.UI (tshow, messageWithButtons, toSelectRequestRecipientButton, formatReceiptItem)
import Telegram.Bot.FSA
  ( State(SelectingReceiptItems, SelectingRequestRecipient)
  , Transition (CancelSelectingRequestRecipient)
  )

handleTransition :: State -> Eff' Transition State
handleTransition (SelectingReceiptItems qr allItems) = case nonEmpty $ allItems & filter fst & map snd of
  Nothing -> SelectingReceiptItems qr allItems <# do
    tg $ replyText "No receipt items selected. Cannot proceed."

  Just items -> SelectingRequestRecipient qr items <# do
    let msgTxt = T.unlines
          $ ("Selected receipt items: " <> tshow (toList items <&> view #index))
          : map (formatReceiptItem . snd) allItems
    tg $ editUpdateMessageText msgTxt
    let receiptTotal = ((`divideDouble` 100) . fromInteger) $ sum $ items <&> view #itemTotal
    let replyMsgText = T.unlines
          $ ("В сумме на: " <> tshow receiptTotal)
          : (view #name <$> toList items)
    token <- authViaTelegram =<< currentUser
    contactsResp <- runReq $ getContacts token
    let contacts = fromResp <$> contactsResp
    let contactsButtons = (:[]) . toSelectRequestRecipientButton <$> contacts
    let bottomButtonRow = [[actionButton "Cancel" CancelSelectingRequestRecipient]]
    let buttons = contactsButtons ++ bottomButtonRow
    let replyMsg = messageWithButtons replyMsgText buttons
    tg $ reply replyMsg

handleTransition _ = error "TODO"
