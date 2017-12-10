import Data.Char
import Data.List.Split

checkSum :: IO Int
checkSum = do
    grid <- readFile "inputs/day2.txt"
    let rows = splitOn "\n" grid
    let info = map (splitOn "\t") rows
    let nums = map (map (\x -> read x :: Int)) info
    let list = map (\x -> (maximum x) - (minimum x)) nums
    let ans  = sum list
    return ans