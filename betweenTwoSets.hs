solve :: [Int] -> [Int] -> Int
-- takes two lists and return the ...
solve as bs = length
        -- length of the elements that are divisors of the bsGcd
        $ filter (\x -> bsGcd `mod` x == 0)
        -- and do this for elements not greater than bsGcd
        $ takeWhile (<= bsGcd)
        -- Map all the multiples Lcm
        $ map (* asLcm) [1..]
        -- Where AsLCm are foldings recursively of as and bs
    where asLcm = foldl1 lcm as
          bsGcd = foldl1 gcd bs

readIntList :: IO [Int]
readIntList = do
    line <- getLine
    return $ map read $ words line

main :: IO ()
main = do
    [n, m] <- readIntList
    as <- readIntList
    bs <- readIntList
    print $ solve as bs
