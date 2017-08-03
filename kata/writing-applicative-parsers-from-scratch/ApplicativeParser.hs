module ApplicativeParser where

import Data.Char
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P $ \s -> map (\(r, a) -> (r, f a)) (unP p s)

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) x = pmap (const x)

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP pred = P f
  where
    f "" = []
    f (s@(c:r)) = if pred c then [(r, c)] else [];

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP c = predP (== c)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P $ \s -> [(s, x)]

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px = P $ \s -> do
    (r, f) <- unP pf s
    (r', a) <- unP px r
    return (r', f a)

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = P $ \s -> do
    (rem, res) <- unP pa s
    (rem', _) <- unP pb rem
    return (rem', res)

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = P $ \s -> do
    (rem, _) <- unP pa s
    unP pb rem

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP s = P $ \x ->
    let len = length s
        pre = take len x
        rem = drop len x
    in  if pre == s
        then [(rem, s)]
        else []

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ \s -> []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
(<<>>) pa pb = P $ \s -> unP pa s ++ unP pb s

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = P $ \s -> f s
  where
    f s = let xs = unP p s
              ys = foldl (++) [] $ map ( \(s', r') -> map ( \(s'', rs) -> (s'', r':rs) )
                                                 (f s') ) xs
          in  if null xs
              then [(s, [])]
              else if null ys
                   then map ( \(s, r) -> (s, [r]) ) xs
                   else ys


-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = P $ \s ->
    let xs = unP p s
    in  if null xs
        then []
        else unP (many p) s


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = map snd $ filter (null . fst) $ unP p cs

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs =
    let res = runParser p cs
    in if length res == 1
       then Just $ head res
       else Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE x) = x
evalExpr (BinOpE AddBO e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (BinOpE MulBO e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (NegE e) = - (evalExpr e)
evalExpr ZeroE = 0

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
--
parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique $ manyWhiteSpace @> parseE <@ manyWhiteSpace

parseE :: Parser Expr
parseE =  parseConst
     <<>> parseBinOpExpr
     <<>> parseNeg
     <<>> parseZero

manyWhiteSpace :: Parser String
manyWhiteSpace = many (charP ' ')

parseZero :: Parser Expr
parseZero = ZeroE <# charP 'z'

parseNeg :: Parser Expr
parseNeg = NegE <#> (charP '-' @> parseE)

parseBinOp :: Parser BinOp
parseBinOp =  (AddBO <# charP '+')
         <<>> (MulBO <# charP '*')

parseBinOpExpr :: Parser Expr
parseBinOpExpr = P $ \s-> do
    (s1, e1) <- unP ( charP '(' @> parseE ) s
    (s2, e2) <- unP ( charP ' ' @> parseBinOp ) s1
    (s3, e3) <- unP ( charP ' ' @> parseE <@ charP ')' ) s2
    return $ (s3, BinOpE e2 e1 e3)

parseConst :: Parser Expr
parseConst = (\x-> ConstE (read x :: Int)) <#> some digit

digit :: Parser Char
digit  =   charP '0'
     <<>>  charP '1'
     <<>>  charP '2'
     <<>>  charP '3'
     <<>>  charP '4'
     <<>>  charP '5'
     <<>>  charP '6'
     <<>>  charP '7'
     <<>>  charP '8'
     <<>>  charP '9'

