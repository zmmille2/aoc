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
    let items      = splitOn "\n" all
    let subitems   = map words items
    let weights    = foldr insertPairs Map.empty (map getWeightPair subitems)
    let familyTree = foldr insertPairs Map.empty (map getFamilyTree subitems)
    let nodes      = map head subitems
    return $ map (getBalance weights familyTree) nodes

getWeightPair (name:weight:_) = (name, (read weight :: Int))

getFamilyTree (name:_:_:children) = (name, children)
getFamilyTree (name:_) = (name, [])

insertPairs (k, v) m = Map.insert k v m

-- so, nodes just straight up doesn't have that format....
--getBalance :: Map k a -> Map k' a' -> [String] -> Int
getBalance weights tree node
    | children == []                                     = weight
    | and $ map (== (head subweights)) (tail subweights) = sum (weight:subweights)
    | otherwise                                          = traceShow ("\n", node, weight, children, subweights, "\n") (sum (weight:subweights))
    where
        children = Map.findWithDefault [] node tree
        weight = Map.findWithDefault 0 node weights
        subweights = map (getBalance weights tree) children