module LispLovesMe where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.List (intercalate)

data AST = I32 Int
         | Sym String
         | Nul
         | Err
         | Lst [AST]
         | Boo Bool
         | Nod AST [AST]
         deriving (Eq, Show)

spaces :: Parser String
spaces = many $ oneOf " ,\r\n\t"

spaces1 :: Parser String
spaces1 = many1 $ oneOf " ,\r\n\t"

parseLisp :: Parser AST
parseLisp =  try parseNod <|> parseExpr

parseNod :: Parser AST
parseNod = do
  spaces
  char '('
  e <- spaces *> parseExpr <* spaces
  es <- parseExpr `sepEndBy` spaces
  spaces
  char ')'
  spaces
  return $ Nod e es

parseExpr :: Parser AST
parseExpr =  spaces *>
        (try parseBoolean
         <|> try parseNull
         <|> parseSymbol
         <|> parseNumber
         <|> parseNod) <* spaces
  where
    parseSymbol = do
      s <- noneOf $ " ,\n\t\r()" ++ [ '0' .. '9' ]
      ss <- many $ noneOf " ,\n\t\r()"
      return $ Sym (s:ss)
    parseNumber = I32 . read <$> many1 digit
    parseBoolean = (string "true" *> return (Boo True)) <|> (string "false" *> return (Boo False))
    parseNull = (string "()" <|> string "null") *> return Nul

-- if Err
eval :: AST -> AST
eval (Nod (Sym f) es) =
  case f `lookup` preludeFunctions of
    Nothing -> Err
    Just func -> func es
eval (Nod _ _) = Err
eval (Lst as) = Lst (map eval as)
eval x = x

is_i32 :: AST -> Bool
is_i32 (I32 _) = True
is_i32 _ = False

is_bool :: AST -> Bool
is_bool (Boo _) = True
is_bool _ = False

is_list :: AST -> Bool
is_list (Lst _) = True
is_list _ = False

seq_op :: (Int -> Int -> Int) -> [AST] -> AST
seq_op f es = let vs = map eval es
              in  if all is_i32 vs
                  then foldl1 (\(I32 x) (I32 y) -> I32 (f x y)) vs
                  else Err

bin_op :: (Int -> Int -> Bool) -> [AST] -> AST
bin_op f es | length es /= 2 = Err
            | otherwise = let vs = map eval es
                          in  if all is_i32 vs
                              then let (I32 x) = head vs
                                       (I32 y) = last vs
                                   in  Boo (f x y)
                              else Err

cond :: [AST] -> AST
cond es | length es < 2 = Err
        | otherwise = let (a:b:c) = map eval es
                      in  if is_bool a
                          then let (Boo cc) = a
                               in  if cc
                                   then b
                                   else if null c
                                        then Nul
                                        else head c
                          else Err

no :: [AST] -> AST
no es | length es /= 1 = Err
      | otherwise = let v = eval (head es)
                    in  if is_bool v
                        then let (Boo x) = v in Boo (not x)
                        else Err

range :: [AST] -> AST
range es | length es /= 2 = Err
         | otherwise = let vs@[v1, v2] = map eval es
                       in  if all is_i32 vs
                           then let (I32 x) = v1
                                    (I32 y) = v2
                                in  Lst $ I32 <$> [x .. y]
                           else Err

size :: [AST] -> AST
size es | length es /= 1 = Err
        | otherwise = let v = eval (head es)
                      in  if is_list v
                          then let (Lst ls) = v in I32 (length ls)
                          else Err

rev :: [AST] -> AST
rev es | length es /= 1 = Err
       | otherwise = let v = eval (head es)
                     in  if is_list v
                         then let (Lst ls) = v in Lst (reverse ls)
                         else Err

preludeFunctions :: [(String, [AST] -> AST)]
preludeFunctions =
  [ ("+", seq_op (+))
  , ("*", seq_op (*))
  , ("-", seq_op (-))
  , ("/", seq_op div)
  , ("^", seq_op (^))
  , (">", bin_op (>))
  , ("<", bin_op (<))
  , ("!", no)
  , ("list", Lst)
  , ("size", size)
  , ("reverse", rev)
  , ("..", range)
  , ("==", bin_op (==))
  , (">=", bin_op (>=))
  , ("<=", bin_op (<=))
  , ("!=", bin_op (/=))
  , ("if", cond)
  ]

lisp2s :: AST -> String
lisp2s (I32 i) = show i
lisp2s (Sym s) = s
lisp2s Nul = "null"
lisp2s (Boo True) = "true"
lisp2s (Boo False) = "false"
lisp2s (Nod e es) = "(" ++ intercalate " " (map lisp2s (e:es)) ++ ")"

lispPretty :: String -> Maybe String
lispPretty s = case parse (parseLisp <* eof) "" s of
  Left _ -> Nothing
  Right ast -> Just $ lisp2s ast

lispEval :: String -> Maybe AST
lispEval s = case parse (parseLisp <* eof) "" s of
  Left _ -> Nothing
  Right ast -> Just (eval ast)