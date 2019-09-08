module ADTS where

data Point = Point Int Int
    deriving Show

getX :: Point -> Int
getX (Point x _) = x

getY :: Point -> Int
getY (Point _ y) = y

moveX :: Int -> Point -> Point
moveX n (Point x y) = Point (x + n) y


data Point' =
    Point' { x :: Int
          , y :: Int
          } deriving (Show, Eq, Ord)

data Bool' = False' | True'
    deriving (Show, Eq, Ord, Enum)

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

if'' :: Bool -> a -> a -> a
if'' theBool whenTrue whenFalse =
    case theBool of
        True -> whenTrue
        False -> whenFalse


infixl 1 ?
(?) :: Bool -> a -> a -> a
(?) = if'

-- >>> zipWith3 if' [True, False] ["a", "b"] ["x", "y"]
-- ["a","y"]
