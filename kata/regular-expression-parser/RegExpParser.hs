module RegExpParser
       ( RegExp(..)
       , parseRegExp
       ) where

import Text.ParserCombinators.Parsec

data RegExp = Normal Char       -- ^ A character that is not in "()*|."
            | Any               -- ^ Any charater
            | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp
            | Or RegExp RegExp  -- ^ A choice between 2 regexps
            | Str [RegExp]      -- ^ A sequence of regexps.
  deriving (Show, Eq)

parseRE :: Parser RegExp
parseRE = do
    r <- parseStr
    try (char '|' *> parseRE >>= \r' -> return (Or r r')) <|> return r

parseStr :: Parser RegExp
parseStr = do
    r <- parseZeroOrMore
    rs <- many parseZeroOrMore
    return $ if null rs then r else Str (r:rs)

parseZeroOrMore :: Parser RegExp
parseZeroOrMore = do
    r <- parseTerm
    try (char '*' *> return (ZeroOrMore r)) <|> return r

parseTerm :: Parser RegExp
parseTerm =   parseNormal
          <|> parseAny
          <|> char '(' *> parseRE <* char ')'

parseNormal :: Parser RegExp
parseNormal = Normal <$> noneOf "()*|."

parseAny :: Parser RegExp
parseAny = char '.' *> return Any

parseRegExp :: String -> Maybe RegExp
parseRegExp s = case parse (parseRE <* eof) "" s of
    Left _ -> Nothing
    Right re -> Just re