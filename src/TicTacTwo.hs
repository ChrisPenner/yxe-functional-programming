module TicTacTwo where

data Board a = Board
  a a
  a a
  deriving (Show, Eq)

data Piece = O | X
  deriving (Show, Eq)

emptyBoard :: Board (Maybe Piece)
emptyBoard =
  Board
    Nothing Nothing
    Nothing Nothing

data Coord = I | II

playPiece :: Piece -> (Coord, Coord) -> Board (Maybe Piece) -> Board (Maybe Piece)
playPiece p (I, I) (Board _ b c d) = Board (Just p) b c d
playPiece p (II, I) (Board a _ c d) = Board a (Just p) c d
playPiece p (I, II) (Board a b _ d) = Board a b (Just p) d
playPiece p (II, II) (Board a b c _) = Board a b c (Just p)

checkWin :: Board (Maybe Piece) -> Maybe Piece
checkWin (Board
  (Just a) _
  _        (Just d))
    | a == d = Just a
checkWin (Board
  (Just a) (Just b)
  _        _)
    | a == b = Just a
checkWin (Board
  (Just a) _
  (Just c) _)
    | a == c = Just a
checkWin (Board
  _ (Just b)
  _ (Just d))
    | b == d = Just b
checkWin _ = Nothing

parseCoord :: String -> Maybe Coord
parseCoord "I" = Just I
parseCoord "II" = Just II
parseCoord _ = Nothing

loop :: Piece -> Board (Maybe Piece) -> IO ()
loop p board = do
    putStrLn "Where would you like to play? e.g. I II"
    ln <- getLine
    case traverse parseCoord (words ln) of
        Just [x, y] -> do
            let newBoard = playPiece p (x, y) board
            case checkWin newBoard of
                Just p -> putStrLn $ show p <> " wins!"
                Nothing -> loop (nextPiece p) newBoard
        _ -> loop p board

nextPiece X = O
nextPiece O = X

