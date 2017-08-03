# Problem

https://www.codewars.com/kata/writing-applicative-parsers-from-scratch

> This Kata will give you an introduction to applicative parsing. First we will
> write some basic combinators and common functions and after that, you will use
> them to parse our own simple kind of arithmetic expressions.

# Parser

## Type

    newtype Parser a = P { unP :: String -> [(String, a)] }

I am always having trouble understanding `newtype` and `record` syntax. Solving
this problem helps me understanding this.

Here `P` is type constructor, which has type `P :: (String -> [(String ,a)]) ->
Parser a`.

`Parser a` has only _one_ record: `unP`, has type `upP :: Parser a -> (String -> [(String
,a)])`.

You can see the symmetry here :smile:

## Feeling

I am having much trouble of when the parser fail the parsing. I have tried several
approaches.

- `error "fail fail fail..."`. not good, terminates after only 1 try
- return `[whole_string, ()]`. what is the empty `Char`? It does not have unit
- return `[remain_string, arbitary_stuff]`. do `length original_string == length
  remain_string`. too much check, bug grows
- return `[]`. good, the final solution. I fail, and return empty, resonable. It
  is the **unit** of list.

Another problem is `many` and `some`.

`many` succeeds even parses nothing. `some` does not, it sould return `[]`. Many
solutions uses:

    many p = inject [] <<>> some p
    some p = ((:) <#> p) <@> many p

but mine is:

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

ugly, right? :sob:

Still far way understanding what the combinator is. // TODO

# Reference

1. https://wiki.haskell.org/Newtype
