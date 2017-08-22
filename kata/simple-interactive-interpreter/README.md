## simple interactive interpreter

https://www.codewars.com/kata/simple-interactive-interpreter

## issues

1. function does not have access to outer variables
2. `Either` Monad for exception handling
3. identifier and empty function call confliction
4. `add add 1 2 add 3 4` parsing. need `env` while parse (before `eval`)

## libs

1. [Data.Map.Strict](http://hackage.haskell.org/packages/archive/containers/latest/doc/html/Data-Map-Strict.html)  `env`
2. [System.Console.Haskeline](http://hackage.haskell.org/package/haskeline) `readline` support
3. [Text.ParserCombinators.Parsec](http://hackage.haskell.org/packages/archive/parsec/latest/doc/html/Text-ParserCombinators-Parsec.html) parsing :simle:
