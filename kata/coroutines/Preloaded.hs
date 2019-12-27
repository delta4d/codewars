{-# LANGUAGE DeriveFunctor #-}

module Preloaded where

newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r } deriving (Functor)

data Command r u d a =
   Done a
 | Out d (Coroutine r u d a)
 | In (u -> Coroutine r u d a) deriving Functor
