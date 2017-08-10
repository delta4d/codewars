# Operator Parser

https://www.codewars.com/kata/operator-parser

# ReadP

[`Text.ParserCombinators.ReadP`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Text-ParserCombinators-ReadP.html)
is a parser combinators provided by base, same as `parsec`, `autoparsec`.

# Associativity

`Associativity` can be processed by `chainl` and `chainr`

# Operator Priority

Operator has different priorities, such as `+` and `*`. Parse lower priority
operator first. Such as `chainl1 term add_op`. Then `term` may contain `*`.

# Parentheses

Parentheses changes priorities. It is whole as a term.

So it is like `Expr = Term | '(' Expr ')'`. And it is a `let me in me` pattern. Recursive!
