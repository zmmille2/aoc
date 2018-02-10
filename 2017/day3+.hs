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
--import Data.Map (Map, lookup)
--import qualified Data.Map as Map

data Direction = R | L | U | D deriving (Show, Eq)
data Location = Location (Int, Int) deriving (Show, Eq)

--getDirection :: Location -> Direction
getDirection (Location (x, y))
    |(x >  y && x >= -y)                     = U
    | x <= y && x >  -y                      = L
    | x <  y && x <= -y                      = D
    | x >= y && x <  -y  || (x, y) == (0, 0) = R

input = 361527

--spiralSolve :: Int -> Int
spiralSolve n = spiralSolveHelper n (Location (0, 1)) [(Location (0, 0), 1)]

--spiralSolveHelper :: Int -> Location -> Map Location Int -> Int
spiralSolveHelper n l m
    | n > 0     = n
    | otherwise = 0
    where
        values = map getValue maybes
        maybes = map (\x -> lookup x m) (getNeighborhood l)

--getNeighborhood :: Location -> [Location]
getNeighborhood (Location (x, y)) = [(Location (x+1, y+1)),
                                     (Location (x,   y+1)),
                                     (Location (x-1, y+1)),
                                     (Location (x+1, y)),
                                     (Location (x,   y)),
                                     (Location (x-1, y)),
                                     (Location (x+1, y-1)),
                                     (Location (x,   y-1)),
                                     (Location (x-1, y-1))]

--getValue :: Maybe Int -> Int
getValue (Just n) = n
getValue Nothing  = 0