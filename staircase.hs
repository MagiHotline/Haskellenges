main :: IO()
main = do
    n <- readLn
    printTwoSeparated n 0

-- Print a staircase of n steps
printTwoSeparated :: Int -> Int -> IO()
printTwoSeparated n m
    | n == 0 = return ()
    | otherwise = do
        -- do it recursively!
        printNTimes " " (n-1)
        printNTimes "#" (m+1)
        putStrLn ""
        printTwoSeparated (n - 1) (m + 1)

-- Print a String n times
printNTimes:: String -> Int -> IO()
printNTimes s n = putStr $ concat $ replicate n s
