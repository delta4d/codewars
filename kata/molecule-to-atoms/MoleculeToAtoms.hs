module MoleculeToAtoms where

import Text.ParserCombinators.Parsec
import qualified Data.Map as M

data AST = Ele String
         | Count AST Int
         | Str [AST]
         deriving (Show)

parseMol :: Parser AST
parseMol = do
    e <- parseCount
    es <- many parseCount
    return $ if null es then e else Str (e:es)

parseCount :: Parser AST
parseCount = do
    e <- parseTerm
    try (parseInt >>= \i -> return (Count e i)) <|> return e
  where
    parseInt = read <$> many1 digit

parseTerm :: Parser AST
parseTerm =  parseEle
         <|> char '(' *> parseMol <* char ')'
         <|> char '[' *> parseMol <* char ']'
         <|> char '{' *> parseMol <* char '}'

parseEle :: Parser AST
parseEle = do
    a <- upper
    try (lower >>= \b -> return (Ele (a:b:""))) <|> return (Ele (a:""))

eval :: AST -> [(String, Int)]
eval ast = M.assocs $ f ast empty
  where
    empty = M.empty :: M.Map String Int
    f :: AST -> M.Map String Int -> M.Map String Int
    f (Ele s) m = M.insertWith (+) s 1 m
    f (Count e i) m = M.unionWith (+) (M.map (* i) (f e empty)) m
    f (Str es) m = M.unionsWith (+) $ map (\e -> f e empty) es

parseMolecule :: String -> Either String [(String, Int)]
parseMolecule s = case parse parseMol "" s of
    Left _ -> Left "Not a valid molecule"
    Right ast -> Right $ eval ast