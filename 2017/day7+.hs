import Data.Char
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

main = do
    all <- readFile "inputs/day7.txt"
    let items    = splitOn "\n" all
    let subitems = map words items
    let weights  = foldr insertPairs Map.empty (map getWeightPair subitems)
    let children = foldr insertPairs Map.empty (map getChildren subitems)
    let nodes = map head subitems
    return map (balanced weights children) nodes

getWeightPair (name:weight:_) = (name, (read weight :: Int))

getChildren (name:_:_:children) = (name, children)
getChildren (name:_) = (name, [])

insertPairs (k, v) m = Map.insert k v m

balanced weights children node