import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split

findDivisible :: [Int] -> Int
findDivisible []     = 0
findDivisible (x:xs)
    | isJust y  = fromJust y
    | otherwise = findDivisible xs
    where y = isDivisible x xs

isDivisible :: Int -> [Int] -> (Maybe Int)
isDivisible n []   = Nothing
isDivisible n (x:xs)
    | n `mod` x == 0 = (Just (quot (max n x) (min n x)))
    | otherwise      = isDivisible n xs

checkSum :: IO Int
checkSum = do
    grid <- readFile "inputs/day2.txt"
    let rows  = splitOn "\n" grid
    let info  = map (splitOn "\t") rows
    let nums  = map (map (\x -> read x :: Int)) info
    let quots = map (findDivisible . reverse . sort) nums
    let ans   = sum quots
    return ans