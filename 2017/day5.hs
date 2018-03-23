import Data.Char
import Data.List
import Data.List.Split
import Control.Lens

runProgram = do
    all <- readFile "inputs/day5.txt"
    let rows = splitOn "\n" all
    let nums = map (\x -> read x :: Int) rows
    return (run 0 nums 0)

run :: Int -> [Int] -> Int -> Int
run index program steps
    | index >= (length program) = steps
    | otherwise                 = run (index') program' (steps + 1)
    where
        value = program!!index
        index' = index + value
        program' = program & element index .~ (value + 1)

-- 1075 too low: forgot to replace index + 1 with index' lol