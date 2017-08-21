{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SimpleInteractiveInterpreter where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.List (sort)
import Data.Char (isSpace)
import qualified Data.Map.Strict as M
import System.Console.Haskeline

-- function        ::= fn-keyword fn-name { identifier } fn-operator expression
-- fn-name         ::= identifier
-- fn-operator     ::= '=>'
-- fn-keyword      ::= 'fn'
--
-- expression      ::= factor | expression operator expression
-- factor          ::= number | identifier | assignment | '(' expression ')' | function-call
-- assignment      ::= identifier '=' expression
-- function-call   ::= fn-name { expression }
--
-- operator        ::= '+' | '-' | '*' | '/' | '%'
--
-- identifier      ::= letter | '_' { identifier-char }
-- identifier-char ::= '_' | letter | digit
--
-- number          ::= { digit } [ '.' digit { digit } ]
--
-- letter          ::= 'a' | 'b' | ... | 'y' | 'z' | 'A' | 'B' | ... | 'Y' | 'Z'
-- digit           ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'

data T = Function String [String] T
       | Operator Char T T
       | Number Double
       | FunctionCall String [T]
       deriving (Show, Eq)

type Env = M.Map String T
type Interpreter = (Env -> Parser T, Env)
type Result = Maybe Double
type EvalResult a = Either String a 

instance Show (Env -> Parser T) where
  show _ = "parser T"

emptyEnv :: Env
emptyEnv = M.empty

newInterpreter :: Interpreter
newInterpreter = (parseT, emptyEnv)

skipSpaces1 :: Parser ()
skipSpaces1 = many1 space *> return ()

skipSpaces :: Parser ()
skipSpaces = many space *> return ()

parseT :: Env -> Parser T
parseT env = (try (parseFunction env) <|> parseExpression env) <* eof

parseNumber :: Env -> Parser T
parseNumber _ = Number . read <$> many1 digit

parseFunCall :: Env -> Parser T
parseFunCall env = do
    name <- parseIdentifier env
    skipSpaces
    args <- case name `M.lookup` env of
              Just (Function _ arg_list _) -> count (length arg_list) (parseExpression env)
              _ -> many (parseExpression env)
    return $ FunctionCall name args

parseIdentifier :: Env -> Parser String
parseIdentifier _ = do
    h <- letter
    r <- many $ alphaNum <|> char '_'
    return (h:r)

parseFunction :: Env -> Parser T
parseFunction env = do
    string "fn"
    skipSpaces1
    name <- parseIdentifier env
    skipSpaces1
    args <- many (parseIdentifier env <* skipSpaces)
    string "=>"
    skipSpaces
    body <- parseExpression env
    return $ Function name args body

parseExpression :: Env -> Parser T
parseExpression env = chainr1 (parseWithoutAssign env) (chainOp "=")
  where
    chainOp :: String -> Parser (T -> T -> T)
    chainOp ops = Operator <$> (skipSpaces *> oneOf ops <* skipSpaces)

    parseWithoutAssign env = chainl1 (parseWithoutAdd env) (chainOp "+-")
    parseWithoutAdd env = chainl1 (parseFactor env) (chainOp "*/%")

parseFactor :: Env -> Parser T
parseFactor env =  skipSpaces *>
              (parseNumber env
           <|> parseFunCall env
           <|> char '(' *> skipSpaces *> parseExpression env <* skipSpaces <* char ')')
            <* skipSpaces

-- data T = Function String [String] T
--        | Operator Char T T
--        | Number Double
--        | FunctionCall String [T]

eval :: T -> Env -> EvalResult (Result, Env)

eval (Number x) env = return (Just x, env)

eval (Operator '=' (FunctionCall name []) e) env = do
  (Just x, env') <- eval e env
  case name `M.lookup` env' of
    Nothing -> return (Just x, M.insert name (Number x) env')
    Just (Number _) -> return (Just x, M.insert name (Number x) env')
    _ -> Left $ name ++ " is already bounded as function"

eval (Operator op e1 e2) env = do
  (Just x1, env')  <- eval e1 env
  (Just x2, env'') <- eval e2 env'
  let x = case op of
            '+' -> x1 + x2
            '-' -> x1 - x2
            '*' -> x1 * x2
            '/' -> x1 / x2
            '%' -> fromIntegral $ (round x1) `mod` (round x2)
  return $ (Just x, env'')

eval func@(Function name args body) env = do
  case name `M.lookup` env of
    Just (Number _) -> Left $ name ++ " is already bounded as identifier"
    _ -> eval (FunctionCall name arg_val) env' >>= \_ -> return $ (Nothing, env')
          where arg_val = map (const $ Number 0.0) args
                env' = M.insert name func env

eval func@(FunctionCall name []) env = do
  case name `M.lookup` env of
    Nothing -> Left $ name ++ " undefined"
    Just (Number x) -> return (Just x, env)
    _ -> eval_func func env

eval func@(FunctionCall name args) env = do
  case name `M.lookup` env of
    Nothing -> Left $ "function " ++ name ++ " not defined"
    Just (Function _ arg_name body) -> eval_func func env

notNum (Number _) = False
notNum _ = True

eval_func :: T -> Env -> EvalResult (Result, Env)
eval_func (FunctionCall name arg_val) env = do
  let Just (Function _ arg_name body) = name `M.lookup` env
  let arg_val_e = map (\x -> let Right (Just y, _) = eval x env in Number y) arg_val
  let arg_env = (M.fromList $ zip arg_name arg_val_e) `M.union` (M.filter notNum env)
  let l_val = length arg_val
  let l_name = length arg_name
  if l_val /= l_name
  then Left $ "argument length not match"
  else eval body arg_env >>= \(x, _) -> return (x, env)

gao :: String -> Interpreter -> Either String (Result, Interpreter)
gao s (p, env) = case parse (p env) "" s of
    Left err -> Left (show err)
    Right tree -> case eval tree env of
                    Left err -> Left err
                    Right (result, env') -> Right (result, (p, env'))

strip :: String -> String
strip = stripHead . stripTail
  where
    stripHead = dropWhile isSpace
    stripTail = reverse . stripHead . reverse

input :: String -> Interpreter -> Either String (Result, Interpreter)
input s int =
    let s' = strip s
    in  if null s'
        then Right (Nothing, int)
        else gao s' int

-- main :: IO ()
-- main = runInputT defaultSettings (repl newInterpreter)
--   where
--     repl int = do
--       (Just line) <- getInputLine ">>> "
--       if line == ":q"
--       then return ()
--       else case input line int of
--                Left e -> outputStrLn ("error " ++ e) >> repl int
--                Right (Nothing, int') -> repl int'
--                Right (Just r, int') -> do
--                   outputStrLn $ show r
--                   repl int'
