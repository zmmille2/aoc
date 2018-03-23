import Data.Char
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

runProgram = do
    all <- readFile "inputs/day5.txt"
    let rows = splitOn "\n" all
    let incr = [0, 1..]
    let nums = map (\x -> read x :: Int) rows
    let inxd = zip incr nums
    return (run 0 0 (Map.fromAscList inxd))

run :: Int -> Int -> Map Int Int -> Int
run index steps program 
    | index >= (Map.size program) = steps
    | otherwise               = run (index') (steps + 1) program'
    where
        value = program Map.! index
        index' = index + value
        program' = (Map.insert index (value + 1) program)

-- 1075 too low: forgot to replace index + 1 with index' lol