import Data.List

-- unwords splits the string into words
main = interact $ unwords . map show . solve . map read . words

solve :: [Int] -> [Int]
solve xs = [min, max]
    where min = sum $ take 4 $ sort xs
          max = sum $ drop 1 $ sort xs
