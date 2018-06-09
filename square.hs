
main = do
    line <- getLine
    if null line
        then do
            putStrLn("Empty input, exit ...")
            return ()
        else do
            let line2 = "[" ++ line ++ "]"
                points = read line2 :: [(Float, Float)]
                res = isSquare points
            putStrLn(res)
            main

-- Test if a square
isSquare :: [(Float, Float)] -> String
isSquare points
    | length points == 4 = let dist = calcSquareDistances points
                               side = take 4 dist
                               diag = drop 4 dist
                           in "True"
    | otherwise = "Not valid input!"

-- Calculate the distances among the points
calcSquareDistances :: [(Float, Float)] -> [Float]
calcSquareDistances points = quicksort [(fst (points!!a)-fst (points!!b))**2 + (snd (points!!a)-snd (points!!b))**2 | a <- [0..3], b <- [a+1..3]]

-- Quicksort algorithm
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted
