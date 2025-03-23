{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.FSA.Transitions.Start (handleTransition) where

import Telegram.Bot.Simple (replyText)

import SmartPrimitives.TextLenRange (unTextLenRange)
import CheckCheck.Contracts.Users (UserResp(..))
import Clients.Backend (getMe)
import Clients.Utils (runReq)
import Telegram.Bot.AppM (currentUser, authViaTelegram, tg, (<#), Eff')
import Telegram.Bot.FSA (State(InitialState), Transition)

handleTransition :: State -> Eff' Transition State
handleTransition InitialState = InitialState <# do
  token <- authViaTelegram =<< currentUser
  UserResp{ username } <- runReq $ getMe token
  tg $ replyText $ "Nice to see you, " <> unTextLenRange username

handleTransition _ = error "TODO"
