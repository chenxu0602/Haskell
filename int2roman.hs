
main = do
    num <- getLine
    if null num 
        then do
            putStrLn("Empty input, exit ...")
            return ()
        else do
            let numInt = read num + 0
                res = int2rom numInt "" [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]  ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]
            putStrLn(res)
            main

int2rom :: Int -> String -> [Int] -> [String] -> String
int2rom a b values numerals
    | a == 0 = b
    | a > 0 = let aa = [x | x <- values, x <= a]
                  m  = length values
                  n  = length aa
              in int2rom (a - aa!!0) (b ++ numerals!!(m-n)) values numerals
    | otherwise = "Something's wrong!"
