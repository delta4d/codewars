module TinyThreePassCompiler where

import Data.List
import Debug.Trace

debug = flip trace

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

data Token = TChar Char
           | TInt Int
           | TStr String
           | NIL
           deriving (Eq, Show)

alpha, digit :: String
alpha = ['a'..'z'] ++ ['A'..'Z']
digit = ['0'..'9']

tokenize :: String -> [Token]
tokenize [] = []
tokenize xxs@(c:cs)
  | c `elem` "-+*/()[]" = TChar c : tokenize cs
  | not (null i) = TInt (read i) : tokenize is
  | not (null s) = TStr s : tokenize ss
  | otherwise = tokenize cs
  where
    (i, is) = span (`elem` digit) xxs
    (s, ss) = span (`elem` alpha) xxs

parse :: [Token] -> AST
parse ts = ast
  where
    (_:args', rest) = break (== TChar ']') ts
    args = init args'
    ast = snd $ parseExp (tail rest)

    op2func :: Token -> (AST -> AST -> AST)
    op2func (TChar '+') = Add
    op2func (TChar '-') = Sub
    op2func (TChar '*') = Mul
    op2func (TChar '/') = Div

    withOp :: [Token] -> ([Token] -> ([Token], AST)) -> ([Token] -> ([Token], [AST -> AST]))
    withOp _ _ [] = ([], [])
    withOp ops p (t:ts)
      | t `elem` ops = let f = op2func t
                           (ts', term) = p ts
                           (ts'', xs) = withOp ops p ts'
                       in  (ts'', flip f term : xs)
      | otherwise = (t:ts, [])

    parseExp :: [Token] -> ([Token], AST)
    parseExp ts = (r', ast)
      where
        (r, x)   = parseTerm ts
        (r', xs) = (withOp [TChar '+', TChar '-'] parseTerm) r
        ast      = foldl (\a b -> b a) x xs

    parseTerm :: [Token] -> ([Token], AST)
    parseTerm ts = (r', ast)
      where
        (r, x)   = parseFactor ts
        (r', xs) = (withOp [TChar '*', TChar '/'] parseFactor) r
        ast      = foldl (\a b -> b a) x xs

    parseFactor :: [Token] -> ([Token], AST)
    parseFactor (t:ts) =
      case t of
        TChar '(' -> let (rest, tree) = parseExp ts in (tail rest, tree)
        TInt x    -> (ts, Imm x)
        x         -> (ts, Arg $ index x args)

    index :: Eq a => a -> [a] -> Int
    index x xs = length (takeWhile (/= x) xs) - 1

compile :: String -> [String]
compile = pass3 . pass2 . pass1

pass1 :: String -> AST
pass1 = parse . tokenize

pass2 :: AST -> AST
pass2 = undefined

pass3 :: AST -> [String]
pass3 = undefined

simulate :: [String] -> [Int] -> Int
simulate asm argv = takeR0 $ foldl' step (0, 0, []) asm where
  step (r0,r1,stack) ins =
    case ins of
      ('I':'M':xs) -> (read xs, r1, stack)
      ('A':'R':xs) -> (argv !! n, r1, stack) where n = read xs
      "SW" -> (r1, r0, stack)
      "PU" -> (r0, r1, r0:stack)
      "PO" -> (head stack, r1, tail stack)
      "AD" -> (r0 + r1, r1, stack)
      "SU" -> (r0 - r1, r1, stack)
      "MU" -> (r0 * r1, r1, stack)
      "DI" -> (r0 `div` r1, r1, stack)
  takeR0 (r0,_,_) = r0