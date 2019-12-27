module Coroutine where

import Control.Monad (ap, forever)
import Preloaded

-- Preloaded contains the following:
-- {-# LANGUAGE DeriveFunctor #-}
--
-- newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r } deriving (Functor)
--
-- data Command r u d a =
--   Done a
-- | Out d (Coroutine r u d a)
-- | In (u -> Coroutine r u d a) deriving Functor

-- Useful alias
apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
apply = runCoroutine

instance Applicative (Coroutine r u d) where
  pure = return
  (<*>) = ap

instance Monad (Coroutine r u d) where
  return x = Coroutine (\k -> k (Done x))
  -- m a -> (a -> m b) -> m b
  f >>= g  = Coroutine $ \k -> apply f $ \command ->
      case command of
        Done a  -> apply (g a) k
        Out d c -> k $ Out d (c >>= g)
        In uc   -> k $ In (\u -> uc u >>= g)

(>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
p1 >>> p2 = Coroutine $ \k -> apply p2 $ \command ->
    case command of
      Done a  -> k $ Done a
      Out d c -> k $ Out d (p1 >>> c)
      In uc   -> apply p1 $ \cmd ->
          case cmd of
            Done a  -> k $ Done a
            Out d c -> apply (c >>> uc d) k
            In uc'  -> k $ In (\u -> uc' u >>> p2)

-- It might be useful to define the following function
-- pipe2 :: Coroutine r u m a -> (m -> Coroutine r m d a) -> Coroutine r u d a

-- Library functions

output :: a -> Coroutine r u a ()
output v = Coroutine $ \k -> k $ Out v $ return ()

input :: Coroutine r v d v
input = Coroutine $ \k -> k $ In $ return

produce :: [a] -> Coroutine r u a ()
produce [] = return ()
produce (x:xs) = output x >> produce xs

consume :: Coroutine [t] u t a -> [t]
consume c = apply c $ \command ->
    case command of
      Done _   -> []
      In   _   -> []
      Out d c' -> d : consume c'

filterC :: (v -> Bool) -> Coroutine r v v ()
filterC p = Coroutine $ \k -> k $ In $ \v ->
    if p v then output v >> filterC p
           else filterC p

limit :: Int -> Coroutine r v v ()
limit n = if n <= 0 then return ()
                    else input >>= output >> limit (n - 1)

suppress :: Int -> Coroutine r v v ()
suppress 0 = input >>= output >> suppress 0
suppress n = input >> suppress (n - 1)

add :: Coroutine r Int Int ()
add = (input >>= \x -> input >>= \y -> output (x + y)) >> add

duplicate :: Coroutine r v v ()
duplicate = (input >>= \x -> output x >> output x) >> duplicate

-- Programs
-- 1. A program which outputs the first 5 even numbers of a stream.
-- 2. A program which produces a stream of the triangle numbers 
-- 3. A program which multiplies a stream by 2
-- 4. A program which sums adjacent pairs of integers

p1, p2, p3, p4 :: Coroutine r Int Int ()

p1 = filterC even >>> limit 5
p2 = produce [ n*(n+1) `div` 2 | n <- [1..] ]
p3 = duplicate >>> add
p4 = duplicate >>> suppress 1 >>> add
