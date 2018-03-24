import Data.Char
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

main = do
    all <- readFile "inputs/day6.txt"
    let items = splitOn "\t" all
    let nums = map (\x -> read x :: Int) items
    let incr = [0, 1..]
    let inxd = zip incr nums
    let dict = Map.fromAscList inxd
    return (step 0 dict Set.empty)

step _ d _ = d