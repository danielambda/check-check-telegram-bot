{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Telegram.Bot.FSA.Transitions.AddContact (handleTransition) where

import Telegram.Bot.Simple (replyText)
import qualified Data.Text as T
import Servant.Auth.Client (Token)
import Servant.API.UVerb (matchUnion, WithStatus)

import SmartPrimitives.TextMaxLen (TextMaxLen(..), mkTextMaxLen)
import CheckCheck.Contracts.Users.Contacts (CreateContactReqBody(..))
import Clients.AuthService (AuthServiceUser(..), getUser, UserQuery (..))
import Clients.Backend (createContact)
import Clients.Utils (runReq, runReq_)
import Telegram.Bot.AppM (currentUser, authViaTelegram, (<#), Eff', tg, AppM)
import Telegram.Bot.FSA (State(InitialState), Transition)

handleTransition :: T.Text -> State -> Eff' Transition State
handleTransition content InitialState = InitialState <# do
  token <- authViaTelegram =<< currentUser
  case T.uncons content of
    Nothing -> tg $ replyText "Please, enter non empty username to add to contacts"
    Just ('@', rest) -> processContact Telegram rest token
    Just _ -> processContact Regular content token

handleTransition _ _ = error "TODO"

processContact :: UsernameType -> T.Text -> Token -> AppM ()
processContact usernameType content token = do
  let (baseUsername, mContactNamePart) = T.span (/= ' ') content
      mContactNameRaw = T.strip mContactNamePart

  contactNameResult <- case mContactNameRaw of
    "" -> pure $ Right Nothing
    name -> case mkTextMaxLen name of
      Just validName -> pure $ Right (Just validName)
      Nothing -> do
        tg $ replyText $ "contact name " <> name <> " is too long, 50 symbols is the max length"
        pure $ Left ()

  case contactNameResult of
    Left _ -> pure ()
    Right contactName -> do
      let (query, formattedUsername) = case usernameType of
            Telegram -> (UserTgUsernameQuery baseUsername, "@" <> baseUsername)
            Regular  -> (UserUsernameQuery baseUsername, baseUsername)
      u <- runReq $ getUser token query
      if| Just AuthServiceUser {userId} <- matchUnion @AuthServiceUser u ->
          handleSuccess userId formattedUsername contactName
        | Just _ <- matchUnion @(WithStatus 404 ()) u ->
          handleUserNotFound formattedUsername usernameType
        | otherwise -> return ()
  where
    handleSuccess contactUserId formattedUsername contactName = do
      runReq_ $ createContact token CreateContactReqBody{..}
      tg $ replyText $ case contactName of
        Just (TextMaxLen name) ->
          "contact " <> formattedUsername <> " successfully added as " <> name
        Nothing ->
          "contact " <> formattedUsername <> " successfully added"

    handleUserNotFound formattedUsername Telegram = do
      tg $ replyText $ T.unlines
        [ "User " <> formattedUsername <> " is not registered in check-check"
        , "Send them the following link to join:"
        ]
      tg $ replyText "https://t.me/CheckCheckTgBot?start=start" -- TODO remove hardlink

    handleUserNotFound formattedUsername Regular =
      tg $ replyText $ "User " <> formattedUsername <> " is not registered in check-check"

data UsernameType = Telegram | Regular
