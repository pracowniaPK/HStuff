-- Sort of Haskell

-- quicksort
sortQS :: Ord a => [a] -> [a]
sortQS [] = []
sortQS (x:xs) = sortQS [y | y <- xs, y <= x] ++ [x] ++ sortQS [y | y <- xs, y > x]
