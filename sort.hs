-- Sort of Haskell

-- quicksort
sortQS :: Ord a => [a] -> [a]
sortQS [] = []
sortQS (x:xs) = sortQS [y | y <- xs, y <= x] ++ [x] ++ sortQS [y | y <- xs, y > x]

-- bubble sort
sortBS :: Ord a => [a] -> [a]
sortBS x
    | length x <= 1 = x
    | otherwise     = sortBS' x 0 True

sortBS' :: Ord a => [a] -> Int -> Bool -> [a]
sortBS' x n True
    | n == length x - 2 = x
sortBS' x n b
    | n <= length x - 2 && x !! n > x !! (n + 1)
                        = sortBS' (swapPair x n) (n + 1) (b && False)
    | n <= length x - 2 = sortBS' x (n + 1) (b && True)
    | otherwise         = sortBS' x 0 True
    where swapPair x n = take n x ++ [x !! (n + 1)] ++ [x !! n] ++ drop (n + 2) x
