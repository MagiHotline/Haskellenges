solve :: [Int] -> [Int]
solve xs = [timesMax, timesMin]
  where
    (timesMax, timesMin, _, _) = foldl trackMaxMin (0, 0, head xs, head xs) xs

-- Accumulator that keeps track of the number of times the max and min values are updated
-- used in the fold function that processes the list of integers
trackMaxMin :: (Int, Int, Int, Int) -> Int -> (Int, Int, Int, Int)
trackMaxMin (countMax, countMin, currentMax, currentMin) x
    | x > currentMax = (countMax + 1, countMin, x, currentMin) -- New max found
    | x < currentMin = (countMax, countMin + 1, currentMax, x) -- New min found
    | otherwise      = (countMax, countMin, currentMax, currentMin) -- No change

main = interact $ unwords . map show . solve . map read . tail . words
