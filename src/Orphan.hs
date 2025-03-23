{-# LANGUAGE LambdaCase, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphan () where

import Telegram.Bot.Simple (GetAction(getNextAction))

instance GetAction a action => GetAction (Maybe a) action where
  getNextAction bot = bot >>= \case
    Just a  -> getNextAction $ pure a
    Nothing -> pure Nothing

