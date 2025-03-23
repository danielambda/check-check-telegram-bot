{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Telegram.Bot.FSA.Transitions.SelectRequestRecipient (handleTransition) where

import Telegram.Bot.Simple (replyText)
import Data.UUID (UUID)

import Data.List.NonEmpty (singleton)

import CheckCheck.Contracts.Users.OutgoingRequests (SendRequestReqBody(..), IndexSelectionReqBody (..))
import Clients.Utils (runReq_)
import Clients.Backend (sendRequest)
import Telegram.Bot.AppM ((<#), tg, Eff', authViaTelegram, currentUser)
import Telegram.Bot.FSA (State(InitialState, SelectingRequestRecipient), Transition)
import Optics ((<&>), view)

handleTransition :: UUID -> State -> Eff' Transition State
handleTransition recipientId (SelectingRequestRecipient receiptQr items) = InitialState <# do
  let indices = items <&> view #index
  token <- authViaTelegram =<< currentUser
  let reqBody = SendReceiptItemsRequestReqBody
        {receiptQr, indexSelections = singleton IndexSelectionReqBody{..}}
  runReq_ $ sendRequest token reqBody
  tg $ replyText "Request successfully sent"

handleTransition _ _ = error "TODO"

