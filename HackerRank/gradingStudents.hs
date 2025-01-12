round5 :: Int -> Int
round5 x
  | x >= 38 && (m5 - x) < 3 = m5
  | otherwise = x
  where
    m5 = x + (5 - x `mod` 5)

gradingStudents :: [Int] -> [Int]
-- remember that the map function takes a function
-- as a first argument and applies that function to every element of the list
gradingStudents xs = map round5 xs

main = interact $ unlines . map show . gradingStudents . map read . tail . words
