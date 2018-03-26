import Data.Char
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

main = do
    all <- readFile "inputs/day6.txt"
    let items = splitOn "\t" all
    let nums = map (\x -> read x :: Int) items
    let incr = [0, 1..]
    let inxd = zip incr nums
    let dict = Map.fromAscList inxd
    return (redistCycle 0 dict (Set.singleton (Map.fromList [(0,10),(1,9),(2,8),(3,7),(4,6),(5,5),(6,4),(7,3),(8,1),(9,1),(10,0),(11,15),(12,14),(13,13),(14,11),(15,12)])))

redistCycle n dict set
    | seen      = traceShow dict n
    | otherwise = redistCycle (n+1) dict'' set'
    where
        seen    = Set.member dict set
        set'    = Set.insert dict set
        (maxIdx, maxVal) = getMax (Map.toList dict) (0, 0)
        dict'   = Map.insert maxIdx 0 dict 
        dict''  = redist dict' maxVal (maxIdx + 1)

getMax :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
getMax [] (f, s)  = (f, s)
getMax (x:xs) (f, s)
    | bigger == s = (f, s)
    | otherwise   = getMax xs x
    where
        bigger = max s (snd (getMax xs x))

redist :: (Map Int Int) -> Int -> Int -> (Map Int Int)
redist dict 0 _  = dict
redist dict value index
    | index >= (Map.size dict) = redist dict value 0
    | otherwise                = redist dict' (value - 1) (index + 1)
    where
        dict' = Map.insertWith (+) index 1 dict