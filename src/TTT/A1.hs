module TTT.A1 where

import Data.Char (toUpper)

-- Q#01

_SIZE_ :: Int
_SIZE_ = 3

-- Q#02

_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03

convertRowIndex :: Char -> Int
convertRowIndex = indexedToA . fromEnum . toUpper
  where indexedToA = flip (-) (fromEnum 'A')

-- Q#04

_INVALID_MOVE_ :: (Int, Int)
_INVALID_MOVE_ = (-1, -1)

-- Q#05

_SEP_ :: String
_SEP_ = "_|_"

-- Q#06

data Square = X | O | E deriving (Eq, Show)

-- Q#07

data GameState = X_WON | O_WON | TIE | IN_PROGRESS
  deriving (Eq, Show)

-- Q#08
type Player = Square
type Row = [Square]
type Line = [Square]
type Board = [Row]
type Move = (Int, Int)

-- Q#09

getFirstPlayer :: Bool -> Player
getFirstPlayer b = if b then X else O

getFirstPlayer_ :: Bool -> Player
getFirstPlayer_ b
  | b = X
  | otherwise = O

-- Q#10

showGameState :: GameState -> String
showGameState state =
  case state of
    X_WON       -> "X won!"
    O_WON       -> "O won!"
    TIE         -> "Tie!"
    IN_PROGRESS -> "Game in progress."


-- Q#11

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X
switchPlayer x = x

-- Q#12

showSquare :: Square -> String
showSquare square
  | square == X = "X"
  | square == O = "O"
  | otherwise   = "_"
