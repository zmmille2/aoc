import Data.Char
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

-- note: compile this one or it will stack overflow lol

main = do
    all <- readFile "inputs/day5.txt"
    let rows = splitOn "\n" all
    let incr = [0, 1..]
    let nums = map (\x -> read x :: Int) rows
    let inxd = zip incr nums
    print (run 0 0 (Map.fromAscList inxd))

run :: Int -> Int -> Map Int Int -> Int
run index steps program 
    | index >= (Map.size program) = steps
    | otherwise               = run (index') (steps + 1) program'
    where
        value = program Map.! index
        index' = index + value
        delta = findDelta value
        program' = (Map.insert index (value + delta) program)

findDelta :: Int -> Int
findDelta n
    | n >= 3    = -1
    | otherwise = 1