import Data.Char
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

-- todo: this is by far the most annoying one so far

main = do
    all <- readFile "inputs/day10.txt"
    let nums = map read $ words all :: [Int]
    return $ knotHash (Map.fromList (zip [0..255] [0..255])) nums 0 0

knotHash :: (Map Int Int) -> [Int] -> Int -> Int -> [Int]
knotHash dict [] _ _ = Map.elems dict
knotHash dict (leng:lengs) position skipSize = knotHash (traceShowId dict') (traceShowId lengs) (traceShowId position') (traceShowId (skipSize + 1))
    where
        position' = mod (position + leng + skipSize) 256
        dict' = tie dict position (leng - 1)

tie :: (Map Int Int) -> Int -> Int -> (Map Int Int)
tie dict pos leng
    | leng <= 0 = dict
    | otherwise = tie dict' pos' leng'
    where
        pos' = mod (pos + 1) 256
        leng' = leng - 2
        dict' = swap dict pos (mod ((pos + leng)) 256)

swap :: (Map Int Int) -> Int -> Int -> (Map Int Int)
swap dict a b = dict'
    where
        dict' = Map.insert b v (Map.insert a v' dict)
        v     = Map.findWithDefault 0 a dict
        v'    = Map.findWithDefault 0 b dict

-- 420 too low, not handling when to start tying correctly...
-- 62250 too high
-- 18632 is not it
-- 20592 is not it
-- 21021 was a typo...
-- 19591 might be it but jeez it's annoying