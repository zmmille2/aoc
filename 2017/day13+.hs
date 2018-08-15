import Data.Char
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

data Direction = Up | Down deriving (Show, Eq)
data State = Guard Int Int Direction deriving (Show) -- position length direction

main = do
    all <- readFile "inputs/day13.txt"
    let info = map (splitOn ":") (lines all)
    let tuples = map toTuple info
    let potentials = [0..]
    return tuples

    -- x + 0 % 4
    -- x = 0, f(x) = 0 | x = 5, f(x) = 1
    -- [0] [1] (2) (3)
    -- x + 1 % 2
    -- x = 0, f(x) = 1 | x = 5, f(x) = 0
    -- [0] (1)
    
toTuple :: [String] -> Maybe (Int, Int)
toTuple (x:y:[]) = Just (x', y')
    where x' = read x :: Int
          y' = read y :: Int
toTuple _    = Nothing

buildFilters :: [Maybe (Int, Int)] -> [a -> Bool -> a -> a]
buildFilters []              = []
buildFilters (Nothing:xs)    = buildFilters xs
buildFilters (Just (x,y):xs) = filter':(buildFilters xs)
    where filter' = (\a, b, c -> (a * c) ) -- do from 20-w/e