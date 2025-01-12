-- Function to read a matrix of integers
readMatrix :: Int -> IO [[Int]]
-- readIntList reads a line of integers and returns a list of integers for each line
readMatrix rows = sequence [readIntList | _ <- [1..rows]]
  where
    readIntList = map read . words <$> getLine

-- Function to calculate the absolute difference of the diagonals
diagDiff :: [[Int]] -> Int
diagDiff xs = abs (sumDiagLeft - sumDiagRight) -- Absolute value of the sums
    where sumDiagLeft = sum $ zipWith (!!) xs [0..] -- The !! operator takes the nth element of a list
          sumDiagRight = sum $ zipWith (!!) xs [length xs - 1, length xs - 2..0]

-- Main function
main = do
    n <- readLn -- Discard the element we don't need
    matrix <- readMatrix n
    putStrLn $ show $ diagDiff matrix
