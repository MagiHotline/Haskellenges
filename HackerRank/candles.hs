main = interact $ show . solve . map read . tail . words

solve :: [Int] -> Int
solve xs = count xs (maximum xs)

-- count how many times an element appears in a list
count :: [Int] -> Int -> Int
count xs x = length $ filter (==x) xs
