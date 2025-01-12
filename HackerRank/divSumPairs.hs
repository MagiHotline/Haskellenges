-- Main function
main = do
    [_, divisor] <- readIntList
    list <- readIntList
    putStrLn $ show $ solve list divisor

solve :: [Int] -> Int -> Int
solve list divisor =
    foldl (\acc (x, y) -> acc + if (x + y) `mod` divisor == 0 then 1 else 0) 0 pairs
    where
        -- We can "spell it out" like this:
        -- The list of all pairs such that i < j (so not necessearily just "i" and "i+1")
        pairs = [(x, y) | (x, i) <- zip list [0..], (y, j) <- zip list [0..], i < j]

-- For [1,3,2,6,1,2] we have the pairs as:
-- "[(1,3),(1,2),(1,6),(1,1),(1,2),(3,2),(3,6),(3,1),(3,2),(2,6),(2,1),(2,2),(6,1),(6,2),(1,2)]"


readIntList :: IO [Int]
readIntList = map read . words <$> getLine
