import Data.Char
import Data.List
import Data.List.Split

runProgram = do
    all <- readFile "inputs/day5.txt"
    let rows = splitOn "\n" all
    let nums = map (\x -> read x :: Int) rows
    return (run 0 nums 0)

run index program steps
    | index > length program = steps
    | otherwise              = run index' program' (steps + 1)
    where
        index' = index + program[index]
        program' = program