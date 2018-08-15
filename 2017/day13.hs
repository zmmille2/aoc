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
    let halls = reverse $ buildList tuples 0 []
    return $ run halls 0 0

updateState :: (Maybe State) -> (Maybe State)
updateState Nothing = Nothing
updateState (Just (Guard pos len dir)) = (Just (Guard pos' len dir'))
    where   dir'
                | pos == 0       = Up
                | pos == len - 1 = Down
                | otherwise      = dir
            pos'
                | dir' == Up   = pos + 1
                | dir' == Down = pos - 1 -- this might be an off-by-one

toTuple :: [String] -> (Int, State)
toTuple (x:y:[]) = (x', y')
    where x'  = read x :: Int
          y'' = read y :: Int
          y'  = (Guard 0 y'' Up)
toTuple _        = (0, (Guard 0 0 Up))

buildList :: [(Int, State)] -> Int -> [Maybe State] -> [Maybe State]
buildList [] _ states = states
buildList a@((next, state):xs) curr states
    | curr == next = buildList xs (curr + 1) ((Just state):states)
    | otherwise    = buildList a (curr + 1) (Nothing:states)

run :: [Maybe State] -> Int -> Int -> Int
run [] _ sev = sev
run (Nothing:xs) weight sev = run xs' weight' sev
    where
        xs' = (map updateState xs)
        weight' = weight + 1
run a@((Just (Guard pos len _)):xs) weight sev = run xs' weight' sev'
    where
        xs' = map updateState xs
        weight' = weight + 1
        sev'
            | pos == 0  = traceShow (head a) (sev + (weight * len))
            | otherwise = sev