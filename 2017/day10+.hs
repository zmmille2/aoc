import Data.Char (ord)
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Data.Bits
import Numeric

main = do
    all <- readFile "inputs/day10.txt"
    let inputs = map ord all
    let length = inputs ++ [17, 31, 73, 47, 23]
    let numRounds = 64
    let sparse = runRounds numRounds length ((Map.fromList (zip [0..255] [0..255]), 0, 0))
    let ordered = Map.toList sparse
    let raw = map snd ordered
    let chunks = chunksOf 16 raw
    return $ map (foldl1' xor) chunks

runRounds :: Int -> [Int] -> ((Map Int Int), Int, Int) -> (Map Int Int)
runRounds numRounds length (dict, position, skipSize)
    | numRounds == 0 = dict
    | otherwise      = runRounds (numRounds - 1) length (dict', position', skipSize')
        where
            (dict', position', skipSize') = knotHash dict length position skipSize

knotHash :: (Map Int Int) -> [Int] -> Int -> Int -> ((Map Int Int), Int, Int)
knotHash dict [] position skipSize = (dict, position, skipSize)
knotHash dict (leng:lengs) position skipSize = knotHash dict' lengs position' (skipSize + 1)
    where
        position' = mod (position + leng + skipSize) 256
        dict' = tie dict position (leng - 1)

tie :: (Map Int Int) -> Int -> Int -> (Map Int Int)
tie dict pos leng
    | leng <= 0 = dict
    | otherwise = tie dict' pos' leng'
    where
        pos'  = mod (pos + 1) 256
        leng' = leng - 2
        dict' = swap dict pos (mod ((pos + leng)) 256)

swap :: (Map Int Int) -> Int -> Int -> (Map Int Int)
swap dict a b = dict'
    where
        dict' = Map.insert b v (Map.insert a v' dict)
        v     = Map.findWithDefault 0 a dict
        v'    = Map.findWithDefault 0 b dict