module Befunge93 where

import System.Random
import qualified Data.Map as M
import Data.Char

type Code = M.Map (Int, Int) Char
--                 rng    dir  position  code stack outstr
data Inter = Inter StdGen Int (Int, Int) Code [Int] String
           deriving (Show) 

xmax :: Int
xmax = 80

ymax :: Int
ymax = 25

emptyCode :: Code
emptyCode = M.fromList [ ((x, y), ' ') | x <- [0..xmax-1], y <- [0..ymax-1] ]

newCode :: String -> Code
newCode s = f 0 0 s emptyCode
  where
    f _ _ "" m = m
    f x y ('\n':s) m = f 0 (y+1) s m
    f x y (c:s) m = f (x+1) y s (M.insert (x, y) c m)

getout :: Inter -> String
getout (Inter _ _ _ _ _ o) = reverse o

op (Inter _ _ pos code _ _) = code M.! pos
push_num c (Inter g dir pos code st o) = Inter g dir pos code (c':st) o
  where c' = digitToInt c
add (Inter g dir pos code (a:b:st) o) = Inter g dir pos code (a+b:st) o
sub (Inter g dir pos code (a:b:st) o) = Inter g dir pos code (b-a:st) o
mul (Inter g dir pos code (a:b:st) o) = Inter g dir pos code (a*b:st) o
did (Inter g dir pos code (a:b:st) o) = Inter g dir pos code (b`div`a:st) o
mou (Inter g dir pos code (a:b:st) o) = Inter g dir pos code (b`mod`a:st) o
no (Inter g dir pos code (a:st) o) = Inter g dir pos code (a':st) o
  where a' = if a == 0 then 1 else 0
gt (Inter g dir pos code (a:b:st) o) = Inter g dir pos code (a':st) o
  where a' = if b > a then 1 else 0
move_right (Inter g dir (x, y) code st o) = Inter g 0 (x', y) code st o
  where x' = if x == xmax - 1 then 0 else x + 1
move_left (Inter g dir (x, y) code st o) = Inter g 1 (x', y) code st o
  where x' = if x == 0 then xmax - 1 else x - 1
move_up (Inter g dir (x, y) code st o) = Inter g 2 (x, y') code st o
  where y' = if y == 0 then ymax - 1 else y - 1
move_down (Inter g dir (x, y) code st o) = Inter g 3 (x, y') code st o
  where y' = if y == ymax - 1 then 0 else y + 1
move_random (Inter g dir pos code st o) = move (Inter g' dir' pos code st o)
  where (dir', g') = randomR (0, 3) g
move int@(Inter _ dir _ _ _ _) = (moves !! dir) int
  where moves = [move_right, move_left, move_up, move_down]
pop_h (Inter g dir pos code (a:st) o) = (if a == 0 then move_right else move_left) (Inter g dir pos code st o)
pop_v (Inter g dir pos code (a:st) o) = (if a == 0 then move_down else move_up) (Inter g dir pos code st o)
scan_string int = f (move int)
  where f int@(Inter g dir pos code st o) =
            let c = op int
                st' = (ord c):st
                int' = Inter g dir pos code st' o
            in  if c == '"' then int else f (move int')
dup (Inter g dir pos code st o) = Inter g dir pos code st' o
  where st' = if null st then [0] else (head st:st)
swap (Inter g dir pos code (a:st) o) = Inter g dir pos code st' o
  where st' = if null st then (a:0:st) else let (b:st'') = st in (b:a:st'')
discard (Inter g dir pos code (_:st) o) = Inter g dir pos code st o
o_i (Inter g dir pos code (a:st) o) = Inter g dir pos code st o'
  where o' = (reverse $ show a) ++ o
o_a (Inter g dir pos code (a:st) o) = Inter g dir pos code st o'
  where o' = (chr a) : o
skip = move
put (Inter g dir pos code (y:x:v:st) o) = Inter g dir pos code' st o
  where code' = M.insert (x, y) (chr v) code
get int@(Inter g dir pos code (y:x:st) o) = Inter g dir pos code st' o
  where st' = let c = code M.! (x, y) in (ord c) : st

runInter :: Inter -> Inter
runInter int@(Inter g dir pos@(x, y) code st o) = int'''
  where
    c = op int
    func = case c of
            '0'  -> push_num c
            '1'  -> push_num c
            '2'  -> push_num c
            '3'  -> push_num c
            '4'  -> push_num c
            '5'  -> push_num c
            '6'  -> push_num c
            '7'  -> push_num c
            '8'  -> push_num c
            '9'  -> push_num c
            '+'  -> add
            '-'  -> sub
            '*'  -> mul
            '/'  -> did
            '%'  -> mou
            '!'  -> no
            '`'  -> gt
            '>'  -> move_right
            '<'  -> move_left
            '^'  -> move_up
            'v'  -> move_down
            '?'  -> move_random
            '_'  -> pop_h
            '|'  -> pop_v
            '"'  -> scan_string
            ':'  -> dup
            '\\' -> swap
            '$'  -> discard
            '.'  -> o_i
            ','  -> o_a
            '#'  -> skip
            'p'  -> put
            'g'  -> get
            _    -> id
    int'   = func int
    int''  = (if c `elem` "><v^?_|" then id else move) int'
    int''' = (if c == '@' then id else runInter) int''

newInter :: StdGen -> String -> Inter
newInter g s = Inter g 0 (0, 0) (newCode s) [] ""

interpret :: StdGen -> String -> String
interpret g s = getout $ runInter $ newInter g s

main :: IO ()
main = do
    s <- getContents
    putStrLn $ show s
    putStrLn $ interpret (mkStdGen 0) s