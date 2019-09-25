module TicTacTwo where

data Piece = O | X
  deriving (Show, Eq)

data Grid a =
    Grid
      a a
      a a
  deriving (Show)

emptyGrid :: Grid (Maybe a)
emptyGrid =
    Grid
        Nothing Nothing
        Nothing Nothing

-- data Maybe a = Just a | Nothing

data Coord = I | II
  deriving Show

placePiece :: Coord -> Coord -> Piece -> Grid (Maybe Piece) -> Maybe (Grid (Maybe Piece))
placePiece x y p (Grid a b c d) =
    case (x, y) of
        (I, I) | a == Nothing -> Just $ Grid (Just p) b c d
        (I, II) | b == Nothing -> Just $ Grid a (Just p) c d
        (II, I) | c == Nothing -> Just $ Grid a b (Just p) d
        (II, II) | d == Nothing -> Just $ Grid a b c (Just p)
        _ -> Nothing

checkWin :: Grid (Maybe Piece) -> Maybe Piece
checkWin (Grid (Just a) (Just b) _ _) | a == b = Just a
checkWin (Grid _ _ (Just c) (Just d)) | c == d = Just c
checkWin (Grid (Just a) _ (Just c) _) | a == c = Just a
checkWin (Grid _ (Just b) _ (Just d)) | b == d = Just b
checkWin _ = Nothing

parseLine :: String -> Maybe (Coord, Coord)
parseLine s =
    case words s of
        ["I", "I"] -> Just (I, I)
        ["II", "I"] -> Just (II, I)
        ["I", "II"] -> Just (I, II)
        ["II", "II"] -> Just (II, II)
        _ -> Nothing


loop :: Piece -> Grid (Maybe Piece) -> IO ()
loop player grid = do
    putStrLn ("Player: " <> show player <> " Where do you want to play?")
    input <- getLine
    case parseLine input of
        Nothing -> loop player grid
        Just (x, y) ->
            case placePiece  x y player grid of
                Nothing -> loop player grid
                Just newGrid -> do
                    case checkWin newGrid of
                        Just winner -> putStrLn (show winner <> " WON!")
                        Nothing -> loop (nextPlayer player) newGrid

nextPlayer :: Piece -> Piece
nextPlayer X = O
nextPlayer O = X

main :: IO ()
main = loop O emptyGrid

