import Data.Char
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

main = do
    all <- readFile "inputs/day11.txt"
    let dirs = splitOn "," all
    let loc  = getLoc 0 0 dirs
    return $ getSteps (0, 0) loc

-- this was immensly helpful http://3dmdesign.com/development/hexmap-coordinates-the-easy-way

getLoc x y []      = (x, y)
getLoc x y (step:rest)
    | step == "n"  = getLoc x (y + 1) rest
    | step == "s"  = getLoc x (y - 1) rest
    | step == "ne" = getLoc (x + 1) (y + 1) rest
    | step == "nw" = getLoc (x - 1) y rest
    | step == "se" = getLoc (x + 1) y rest
    | step == "sw" = getLoc (x - 1) (y - 1) rest

getSteps (x0, y0) (x1, y1) = maximum [abs dx, abs dy, abs dd]
    where
        dx = x1 - x0
        dy = y1 - y0
        dd = dx - dy
