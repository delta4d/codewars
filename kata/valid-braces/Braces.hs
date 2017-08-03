validBraces :: String -> Bool
validBraces = f []
  where
    isOpen x    = x == '(' || x == '{' || x == '['
    isMatch x y = x == '(' && y == ')' || x == '{' && y == '}' || x == '[' && y == ']'
    f [] [] = True
    f _  [] = False
    f st (x:xs) =
        if   isOpen x
        then f (x:st) xs
        else not (null st) && let y:ys = st in isMatch y x && f ys xs
