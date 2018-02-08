import Data.Map (Map)
import qualified Data.Map as Map

input = 361527

spiralSolve :: Int -> Int
spiralSolve n = spiralSolveHelper n Map.empty

spiralSolveHelper :: Int -> Map Location Int -> Int
spiralSolveHelper n _ = n
--spiralSolveHelper n (Location (x, y) _) foo = n

-- I think I should just pass location around and have checks of when to switch location... is that easy to do though?
-- can I take a function that takes a location and returns a direction?
-- if I just use coordinates, this becomes relatively easy....
--          \ B  /
--           \  /
--         C  \/ A
--            /\
--           /  \
--          /  D \

-- A = X > Y  && X >= -Y
-- B = X <= Y && X > -Y
-- C = X < Y  && X <= -Y
-- D = X >= Y && X <  -Y

-- the below is promising and the problem will fall out of this way of thinking, but Location might not want a value, that might be stored in a map somehow
-- this would be to allow me to grab values from relative locations: I can't do that if I don't have some handle on the objects

data Direction = R | L | U | D deriving (Show)
data Location = Location (Int, Int) deriving (Show)

getDirection :: Location -> Direction
getDirection (Location (x, y))
    |(x >  y && x >= -y)                     = U
    | x <= y && x >  -y                      = L
    | x <  y && x <= -y                      = D
    | x >= y && x <  -y  || (x, y) == (0, 0) = R