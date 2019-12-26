{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}

module Kata.TimesComm where

import Kata.TimesComm.Definitions

{- Preloaded code. Maybe helpful for local editing.

-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
  EqlZ :: Equal Z Z
  EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- | Peano definition of multiplication.
type family (:*:) (n :: *) (m :: *) :: *
type instance Z :*: m = Z
type instance S n :*: m = m :+: (n :*: m)

-}

-- helpers

reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS (reflexive n)

symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS a) = EqlS (symmetric a)

transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS a) (EqlS b) = EqlS (transitive a b)

rightS :: Natural a -> Natural b -> Equal (S (a :+: b)) (a :+: S b)
rightS NumZ b = EqlS (reflexive b)
rightS (NumS a) b = EqlS (rightS a b)

-- This will be helpful
plus' :: Equal n m -> Natural a -> Equal (n :+: a) (m :+: a)
plus' EqlZ a = reflexive a
plus' (EqlS s) a = EqlS (plus' s a)

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-plus-c-equals-a-plus-b-plus-c-prove-it/haskell
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc NumZ NumZ c = reflexive c
plusAssoc NumZ (NumS b) c = EqlS (plusAssoc NumZ b c)
plusAssoc (NumS a) b c = EqlS (plusAssoc a b c)

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-equals-b-plus-a-prove-it/haskell
plusComm :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusComm NumZ NumZ = EqlZ
plusComm NumZ (NumS b) = EqlS (plusComm NumZ b)
plusComm (NumS a) b = transitive (EqlS (plusComm a b)) (rightS b a)

-- This will also be helpful
zeroComm :: Natural a -> Equal Z (a :*: Z)
zeroComm NumZ = EqlZ
zeroComm (NumS a) = zeroComm a

-- helpers
(<=>) = transitive

(+++) :: Natural m -> Natural n -> Natural (m :+: n)
(+++) NumZ n = n
(+++) (NumS m) n = NumS (m +++ n)

(***) :: Natural m -> Natural n -> Natural (m :*: n)
(***) NumZ n = NumZ
(***) (NumS m) n = n +++ (m *** n)

plus'' :: Natural a -> Equal m n -> Equal (a :+: m) (a :+: n)
plus'' NumZ e = e
plus'' (NumS a) e = EqlS (plus'' a e)

-- This is the proof that the kata requires.
-- | a * b = b * a
timesComm :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
timesComm NumZ b = zeroComm b
timesComm a NumZ = symmetric (zeroComm a)
timesComm sa@(NumS a) sb@(NumS b) = EqlS $
    plus'' b (timesComm a sb)           <=>
    plus'' b (plus'' a (timesComm b a)) <=>
    plusAssoc b a (a *** b)             <=>
    plus' (plusComm b a) (a *** b)      <=>
    symmetric (plusAssoc a b (a *** b)) <=>
    plus'' a (timesComm sa b)

-- (S a) * (S b)
-- (S b) + a * (S b)
-- S $ b + a * (S b)
-- S $ b + (S b) * a
-- S $ b + (a + b * a)
-- S $ b + (a + a * b)
-- S $ (b + a) + a * b
-- S $ (a + b) + a * b
-- S $ a + (b + a * b)
-- S $ a + (S a) * b
-- S $ a + b * (S a)
-- (S a) + b * (S a)
-- (S b) * (S a)
