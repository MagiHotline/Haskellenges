solve :: [Int] -> [Double]
solve xs = [p,n,z]
    where
        p = frequency xs (filter (>0) xs)
        n = frequency xs (filter (<0) xs)
        z = frequency xs (filter (==0) xs)

frequency :: [Int] -> [Int] -> Double
frequency totalList partialList = fromIntegral (length partialList) / fromIntegral (length totalList)

main = interact $ unlines . map show . solve . map read . tail . words
