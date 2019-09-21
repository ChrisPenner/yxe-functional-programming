{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module TicTacToe where

import Control.Applicative

data Board a =
    Board (Row (Row a))
  deriving (Show, Eq)

data Piece = X | O
    deriving (Show, Eq)

data Row a = Row a a a
    deriving (Show, Eq, Functor)

data Coord = I | II | III
    deriving (Show, Eq)

showBoard :: Show a => Board (Maybe a) -> String
showBoard (Board (Row ra rb rc)) = unlines
  [ showRow ra
  , showRow rb
  , showRow rc
  ]
    where
      showRow (Row a b c) = unwords
        [ maybe "." show a
        , maybe "." show b
        , maybe "." show c
        ]



updateRow ::  Coord -> a -> Row a -> Row a
updateRow I a (Row _ b c) = Row a b c
updateRow II b (Row a _ c) = Row a b c
updateRow III c (Row a b _) = Row a b c

getRow :: Coord -> Row a -> a
getRow I (Row a _ _) = a
getRow II (Row _ b _) = b
getRow III (Row _ _ c) = c

play :: a -> (Coord, Coord) -> Board a -> Board a
play a (x, y) (Board pieces) =
    Board $ updateRow x (updateRow y a (getRow x pieces)) pieces

checkWin :: Board (Maybe Piece) -> Maybe Piece
checkWin b = checkHorizontalWin b <|> checkVerticalWin b <|> checkDiagonalWin b

testBoard :: Board (Maybe Piece)
testBoard = Board $ Row
  (Row Nothing (Just O) Nothing)
  (Row Nothing (Just O) Nothing)
  (Row Nothing (Just O) Nothing)

checkDiagonalWin :: Board (Maybe Piece) -> Maybe Piece
checkDiagonalWin
    (Board (Row
      (Row (Just x) _        _      )
      (Row _        (Just y) _      )
      (Row _        _        (Just z))
           )) | x == y && y == z = Just x
checkDiagonalWin
    (Board (Row
      (Row _        _        (Just x))
      (Row _        (Just y) _      )
      (Row (Just z) _        _       )
           )) | x == y && y == z = Just x
checkDiagonalWin _ = Nothing


checkHorizontalWin, checkVerticalWin :: Eq a => Board (Maybe a) -> Maybe a
checkHorizontalWin (Board (Row ra rb rc)) = allEq ra <|> allEq rb <|> allEq rc
checkVerticalWin (Board rows) =
        allEq (getRow I <$> rows)
    <|> allEq (getRow II <$> rows)
    <|> allEq (getRow III <$> rows)

allEq :: Eq a => Row (Maybe a) -> Maybe a
allEq (Row a b c) | a == b && a == c = a
                  | otherwise = Nothing

emptyBoard :: Board (Maybe a)
emptyBoard = Board $ Row
  (Row Nothing Nothing Nothing)
  (Row Nothing Nothing Nothing)
  (Row Nothing Nothing Nothing)

toCoord :: String -> Maybe Coord
toCoord "1" = Just I
toCoord "2" = Just II
toCoord "3" = Just III
toCoord _= Nothing

playGame :: Piece -> Board (Maybe Piece) -> IO ()
playGame piece board = do
    putStrLn $ show piece <> ", where do you want to play? E.g. '1 2'"
    ln <- getLine
    let coords = case words ln of
                [a, b] -> do
                    firstCoord <- toCoord a
                    secondCoord <- toCoord b
                    return (firstCoord, secondCoord)
                _ -> Nothing
    case coords of
        Nothing -> playGame piece board
        Just coords' -> do
            let newBoard = play (Just piece) coords' board
            case checkWin newBoard of
                Nothing -> do
                    putStrLn $ "======"
                    putStrLn $ showBoard newBoard
                    playGame (otherPiece piece) newBoard
                Just w -> putStrLn $ show w <> " won the game!"

main :: IO ()
main = playGame X emptyBoard

otherPiece :: Piece -> Piece
otherPiece X = O
otherPiece O = X


