{-# LANGUAGE DeriveFunctor #-}

module OperatorParser
    ( OpTree(..)
    , Associativity(..)
    , op
    , foldTree
    , parseOperators
    , module Text.ParserCombinators.ReadP
    )
where

import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)

-- | Type for operator parse results. 'a' is the type of the operator, 'b'
-- | of the terms.
data OpTree a b = Op (OpTree a b) a (OpTree a b)
                | Term b
                deriving (Show, Eq, Functor)

-- | Type for specifying the assocativity of operators: left, right, or none.
data Associativity a = L a | R a | NoAssociativity a
                     deriving (Show, Eq, Functor)

-- | Transform an OpTree using the given function.
foldTree :: (a -> b -> b -> b) -> OpTree a b -> b
foldTree f (Term b) = b
foldTree f (Op t1 a t2) = f a (foldTree f t1) (foldTree f t2)

-- | Return a parser such that: given 'op s a', if s matches, the parser
-- | returns a
op :: String -> a -> ReadP a
op s a = fmap (const a) (string s)

parentheses :: ReadP a -> ReadP a
parentheses p = char '(' *> skipSpaces *> p <* skipSpaces <* char ')'

-- | Accept two arguments:
-- | (1) A list of type [Associativity [ReadP a]], which contains parsers for
-- | operators (ReadP a). Each item of type Associativity [ReadP a] contains
-- | a group of operator parsers of the same precedence and associativity;
-- | these groups are listed in order of precedence (lowest to highest).
-- | (2) A parser for the terms.
-- | And return a parser for operator expressions that yields a parse tree.
parseOperators :: [Associativity [ReadP a]] -> ReadP b -> ReadP (OpTree a b)
parseOperators y term =
    let me = op y term
        op [] term = fmap Term term
        op (x:xs) term =
            case x of
                L as -> mkChain chainl1 as
                R as -> mkChain chainr1 as
                NoAssociativity as -> mkChain chain2 as
            where
                term' = op xs term +++ parentheses me
                mkChain chain as =
                    let asp = foldl1 (+++) as
                        ops = skipSpaces *> asp <* skipSpaces >>= \a -> return (\t1 t2 -> Op t1 a t2)
                    in  chain term' ops
    in  me

chain2 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chain2 p op = do { a <- p; rest a }
  where
    rest a = (do f <- op
                 b <- p
                 return (f a b))
          +++ return a
