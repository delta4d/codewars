{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

---------------------------------------------------

toPair :: SPair a b -> (a,b)
toPair p = runPair p (,)

fromPair :: (a,b) -> SPair a b
fromPair (a, b) = SPair (\f -> f a b)

fst :: SPair a b -> a
fst p = runPair p (\x _ -> x)

snd :: SPair a b -> b
snd p = runPair p (\_ x -> x)

swap :: SPair a b -> SPair b a
swap p = runPair p (\x y -> fromPair (y, x))

curry :: (SPair a b -> c) -> (a -> b -> c)
curry f a b = f (fromPair (a, b))

uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f p = runPair p f

---------------------------------------------------

toMaybe :: SMaybe a -> Maybe a
toMaybe (SMaybe m) = m Nothing Just

fromMaybe :: Maybe a -> SMaybe a
fromMaybe Nothing = SMaybe (\x _ -> x)
fromMaybe (Just a) = SMaybe (\_ f -> f a)

isJust :: SMaybe a -> Bool
isJust (SMaybe m) = m False (const True)

isNothing :: SMaybe a -> Bool
isNothing (SMaybe m) = m True (const False)

catMaybes :: SList (SMaybe a) -> SList a
catMaybes ls = runList ls nill (\a sa ->
                    let rest = catMaybes sa
                    in case toMaybe a of
                           Nothing -> rest
                           Just a -> cons a rest)

---------------------------------------------------

toEither :: SEither a b -> Either a b
toEither (SEither e) = e Left Right

fromEither :: Either a b -> SEither a b
fromEither (Left a) = SEither (\f _ -> f a)
fromEither (Right b) = SEither (\_ f -> f b)

isLeft :: SEither a b -> Bool
isLeft (SEither e) = e (const True) (const False)

isRight :: SEither a b -> Bool
isRight (SEither e) = e (const False) (const True)

partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition ls = fromPair (f ls)
  where
    f ls = runList ls (nill, nill) (\a sa ->
            let (l, r) = f sa
            in case toEither a of
                  Left l' -> (cons l' l, r)
                  Right r' -> (l, cons r' r))

---------------------------------------------------

(++) :: [a] -> [a] -> [a]
(++) [] b = b
(++) (a:as) b = a : (as ++ b)

nill :: SList a
nill = SList (\x _ -> x)

cdr :: SList a -> SList a
cdr ls = runList ls nill (\_ x -> x)

toList :: SList a -> [a]
toList ls = runList ls [] (\a sa -> a : toList sa)

fromList :: [a] -> SList a
fromList [] = nill
fromList (a:as) = cons a (fromList as)

cons :: a -> SList a -> SList a
cons a l = SList (\_ f -> f a l)

concat :: SList a -> SList a -> SList a
concat a b = fromList (toList a ++ toList b)

null :: SList a -> Bool
null (SList l) = l True (const (const False))

length :: SList a -> Int
length ls = runList ls 0 (\_ sa -> 1 + length sa)

map :: (a -> b) -> SList a -> SList b
map f ls = runList ls nill (\a sa -> cons (f a) (map f sa))

zip :: SList a -> SList b -> SList (SPair a b)
zip la lb = fromList $ f (toList la) (toList lb) 
  where
    f [] _ = []
    f _ [] = []
    f (x:xs) (y:ys) = fromPair (x, y) : f xs ys

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f x ls = runList ls x (\a sa -> foldl f (f x a) sa)

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f x ls = runList ls x (\a sa -> f a (foldr f x sa))

take :: Int -> SList a -> SList a
take 0 ls = nill
take n ls = runList ls nill (\a sa -> cons a $ take (n-1) sa)