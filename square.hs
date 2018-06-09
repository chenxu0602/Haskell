
main = do
    line <- getLine
    if null line
        then do
            putStrLn("Empty input, exit ...")
            return ()
        else do
            let line2 = "[" ++ line ++ "]"
                points = read line2 :: [(Float, Float)]
                distances = calcSquareDistances points
                res = isSquare distances
            putStrLn(show res)
            main

-- Test if a square by comparing the sides and diagnals
isSquare :: [Float] -> Bool
isSquare dist
    | length dist == 6 = let sides = take 4 dist
                             diags = drop 4 dist
                             sideAndDiag = [sides!!0 * 2, diags!!0]
                         in if sides!!0 > 0 && isEq sides && isEq diags && isEq sideAndDiag then True else False
    | otherwise = False

-- Calculate the square of the distances among the points
calcSquareDistances :: [(Float, Float)] -> [Float]
calcSquareDistances points 
    | length points == 4 = quicksort [(fst (points!!a)-fst (points!!b))**2 + (snd (points!!a)-snd (points!!b))**2 | a <- [0..3], b <- [a+1..3]] 
    | otherwise = [-1]

-- Quicksort algorithm
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

-- Check if a list of float are the same/close enough (with a tolerance of 1e-5)
isEq :: [Float] -> Bool
isEq [] = True
isEq [x] = True
isEq (x:xs) = let n = length [a | a <- xs, abs(a-x) < 1e-5] 
              in n == length xs
