-- Main function
main = do
    n <- getLine -- Discard the element we don't need
    chocBar <- readIntList
    [day, month] <- readIntList
    putStrLn $ show $ solve chocBar day month

-- Count how many subarrays of length month sum up to day.
solve :: [Int] -> Int -> Int -> Int
-- We iterate in the list of integers and we calculate the sum of the month segments
-- [0 .. length chocBar - month] we iterate through this indexes(?)
solve chocBar day month = foldr (\i acc -> acc + findSumThroughDayAndMonth (drop i chocBar) day month) 0 [0 .. length chocBar - month]

readIntList :: IO [Int]
readIntList = map read . words <$> getLine

findSumThroughDayAndMonth :: [Int] -> Int -> Int -> Int
findSumThroughDayAndMonth xs day month = if sum (take month xs) == day then 1 else 0
