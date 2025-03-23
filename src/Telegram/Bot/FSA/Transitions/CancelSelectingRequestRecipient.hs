{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.FSA.Transitions.CancelSelectingRequestRecipient (handleTransition) where

import qualified Data.Text as T
import Optics (view, (<&>))
import Telegram.Bot.Simple (editUpdateMessageText)

import Data.Foldable (toList)
import GHC.Float (divideDouble)

import Telegram.Bot.AppM (Eff', (<#), tg)
import Telegram.Bot.UI (tshow)
import Telegram.Bot.FSA (State(InitialState, SelectingRequestRecipient), Transition)

handleTransition :: State -> Eff' Transition State
handleTransition (SelectingRequestRecipient _ items) = InitialState <# do
  let receiptTotal = ((`divideDouble` 100) . fromInteger) $ sum $ items <&> view #itemTotal
  let replyMsg = T.unlines
        $ ("В сумме на: " <> tshow receiptTotal)
        : (view #name <$> toList items)
  tg $ editUpdateMessageText replyMsg

handleTransition _ = error "TODO"
