module Telegram.Bot.FSA.Transitions.Id (handleTransition) where

handleTransition :: Applicative f => a -> f a
handleTransition = pure
