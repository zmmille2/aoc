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

-- the last error was realizing that the x=-y line for x < 0 is an edge case! Direction should be R for both locations on the border lines

data Direction = R | L | U | D deriving (Show, Eq)
data Location = Location (Int, Int) deriving (Show, Eq)

getDirection :: Location -> Direction
getDirection (Location (x, y))
    | x >  y && x >  -y                      = U
    | x <= y && x >  -y                      = L
    | x <  y && x <= -y                      = D
    | x >= y && x <= -y  || (x, y) == (0, 0) = R

spiralSolve :: Int -> Int
spiralSolve n = spiralSolveHelper n (Location (0, 0)) [((Location (0, 0)), 1)]

spiralSolveHelper :: Int -> Location -> [(Location, Int)] -> Int
spiralSolveHelper n l m
    | n < value = value
    | otherwise = spiralSolveHelper n l' m'
    where
        result = lookup l m
        value  = getValue result
        l' = getNext l
        maybes = map (\x -> lookup x m) (getNeighborhood l')
        values = map getValue maybes
        m' = (l', sum values):m

getNeighborhood :: Location -> [Location]
getNeighborhood (Location (x, y)) = [(Location (x+1, y+1)),
                                     (Location (x,   y+1)),
                                     (Location (x-1, y+1)),
                                     (Location (x+1, y)),
                                     (Location (x,   y)),
                                     (Location (x-1, y)),
                                     (Location (x+1, y-1)),
                                     (Location (x,   y-1)),
                                     (Location (x-1, y-1))]

getValue :: Maybe Int -> Int
getValue (Just n) = n
getValue Nothing  = 0

getNext :: Location -> Location
getNext l@(Location (x, y))
    | direction == R = Location (x + 1, y)
    | direction == L = Location (x - 1, y)
    | direction == U = Location (x, y + 1)
    | direction == D = Location (x, y - 1)
    where
        direction = getDirection l