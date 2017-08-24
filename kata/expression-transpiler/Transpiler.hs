module Transpiler where

import Data.Char
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec
import System.Console.Haskeline

data F = Function F [F]
       | Identifier String
       | Lambda [F] [F]
       deriving (Show)

-----------------------------------------------------------------

skipSpaces :: Parser ()
skipSpaces = many space *> return ()

skipSpaces1 :: Parser ()
skipSpaces1 = many1 space *> return ()

parseFunction :: Parser F
parseFunction = do
    skipSpaces
    f <- parseExpression
    skipSpaces
    fs <- try withPara <|> lambda_a
    skipSpaces
    return $ Function f fs
  where
    withPara = (++) <$> (char '(' *> skipSpaces *> parsePara <* skipSpaces <* char ')') <*> (try lambda_a <|> return [])
    lambda_a = (skipSpaces *> parseLambda <* skipSpaces) >>= \x -> return [x]

parseExpression :: Parser F
parseExpression = parseId <|> parseLambda

parsePara :: Parser [F]
parsePara = (parseExpression <* skipSpaces) `sepBy` (char ',' <* skipSpaces)

parseId :: Parser F
parseId = parseName <|> parseNumber
  where
    parseName = Identifier <$> ( (:) <$> (char '_' <|> letter) <*> many (char '_' <|> alphaNum) )
    parseNumber = Identifier <$> many1 digit

parseLambda :: Parser F
parseLambda = do
    char '{'
    skipSpaces
    para <- try parseLambdaParam <|> return []
    skipSpaces
    stmt <- try parseLambdaStmt  <|> return []
    skipSpaces
    char '}'
    skipSpaces
    return $ Lambda para stmt
  where
    parseLambdaParam = (parseId <* skipSpaces) `sepBy1` (char ',' <* skipSpaces) <* (skipSpaces *> string "->" <* skipSpaces)
    parseLambdaStmt = many1 (parseId <* skipSpaces)

-----------------------------------------------------------------

eval :: F -> String
eval (Identifier id) = id

eval (Function func params) = eval func ++ "(" ++ params_s  ++ ")"
  where
    params_a = map eval params
    params_s = intercalate "," params_a

eval (Lambda params stmts) = "(" ++ params_s ++ "){" ++ stmts_s ++ "}"
  where
    params_s = intercalate "," $ map eval params
    stmts_s = let s = intercalate ";" $ map eval stmts
              in  if null s then s else s ++ ";"

transpile :: String -> Either String String
transpile s = case parse (parseFunction <* eof) "" s of
    Left _  -> Left "Hugh?"
    Right f -> Right $ eval f

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        _s <- getInputLine "> "
        case _s of
            Nothing -> return ()
            Just s -> case parse parseFunction "" s of
                        Left e -> outputStrLn $ show e
                        Right f -> outputStrLn $ eval f
        loop