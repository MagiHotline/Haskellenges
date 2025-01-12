-- Read an array of integers from a line
readIntList :: IO [Int]
readIntList = map read . words <$> getLine

-- Get the list from the input and return the result
main :: IO ()
main = do
    a <- readIntList
    b <- readIntList
    let [pointsA, pointsB] = solve a b
    putStrLn $ show pointsA ++ " " ++ show pointsB

-- Compare the two lists and return the result
solve :: [Int] -> [Int] -> [Int]
solve a b = [pointsA, pointsB]
    where pointsA = length $ filter (== 1) $ zipWith (\x y -> if x > y then 1 else 0) a b
          pointsB = length $ filter (== 1) $ zipWith (\x y -> if x < y then 1 else 0) a b
