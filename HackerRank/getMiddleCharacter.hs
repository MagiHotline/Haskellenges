getMiddleChar:: String -> String
getMiddleChar s
    | even $ length s = [s !! (length s `div` 2 - 1), s !! (length s `div` 2)]
    | otherwise = [s !! (length s `div` 2 - 1)]
