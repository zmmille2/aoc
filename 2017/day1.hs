import Data.Char

repeatSum :: String -> Int
repeatSum [] = 0
repeatSum xs = repeatSumHelper ys 0
    where ys = (last xs):xs

repeatSumHelper :: String -> Int -> Int
repeatSumHelper [] runningTotal = runningTotal
repeatSumHelper (x:(y:xs)) runningTotal
    | x == y    = repeatSumHelper (y:xs) ((digitToInt y) + runningTotal)
    | otherwise = repeatSumHelper (y:xs) runningTotal
repeatSumHelper _ runningTotal = runningTotal