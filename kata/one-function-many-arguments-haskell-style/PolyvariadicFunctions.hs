{-# OPTIONS -XFunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

module PolyvariadicFunctions where

class PolyAdd a where
    add' :: Int -> a

instance PolyAdd Int where
    add' = id

instance (Integral a, PolyAdd r) => PolyAdd (a -> r) where
    add' x y = add' (x + (fromIntegral y :: Int))

polyAdd :: PolyAdd r => r
polyAdd = add' 0

--------------------------------------

class PolyWords a where
    words' :: String -> a

instance PolyWords String where
    words' x = if null x then x else tail x

instance PolyWords r => PolyWords (String -> r) where
    words' x y = words' (x ++ " " ++ y)

polyWords :: PolyWords a => a
polyWords = words' ""

---------------------------------------

class PolyList a r | r -> a where
    list' :: [a] -> r

instance PolyList a [a] where
    list' = id

instance PolyList a r => PolyList a (a -> r) where
    list' l x = list' (l ++ [x])

polyList :: PolyList a r => r
polyList = list' []