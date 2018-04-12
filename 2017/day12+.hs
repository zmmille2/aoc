import Data.Char
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

main = do
    all <- readFile "inputs/day12.txt"
    let regs   = map (!! 0) (map (splitOn " <-> ") (lines all))
    let vals   = map (splitOn ", " . (\x -> x !! 1)) (map (splitOn " <-> ") (lines all))
    let pipes  = Map.fromList (zip regs vals)
    let groups = map (\x -> findTotal [x] pipes Set.empty) regs
    return $ Set.size (Set.fromList groups)

findTotal [] _ total = total
findTotal (x:xs) pipes total
    | Set.member x total = findTotal xs pipes total
    | otherwise          = findTotal xs' pipes total'
        where
            total' = Set.insert x total
            xs'    = (Map.findWithDefault [x] x pipes) ++ xs