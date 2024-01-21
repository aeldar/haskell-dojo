module TTT.A2 where

import Data.List (intercalate)
import TTT.A1

-- Q#01

promptPlayer :: Player -> String
promptPlayer p =
  concat [
    "Player "
    , show p
    , "'s turn: enter a row and column position (ex. A1)"
  ]

-- Q#02

_RANGE_ :: [Int]
_RANGE_ = [0 .. _SIZE_ - 1]

-- Q#03

isDigit :: Char -> Bool
isDigit c = c `elem` [ '0' .. '9' ]

readDigit :: Char -> Int
readDigit c
  | isDigit c = read [c]
  | otherwise = -1

-- Q#04

_EMPTY_ROW_ :: Row
_EMPTY_ROW_ = replicate _SIZE_ E

_EMPTY_BOARD_ :: Board
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05

isTied :: Board -> Bool
isTied board = not $ E `elem` flattenBoard
  where
    flattenBoard = foldr (++) [] board

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]

-- Q#06

indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings = zip ['A'..]

-- Q#07

formatLine :: [String] -> String
formatLine = wrap . intercalate _SEP_
  where
    wrap :: String -> String
    wrap s = concat [_SEP_, s, _SEP_]

-- Q#08

isMoveInBounds :: Move -> Bool
isMoveInBounds (row, col) = all id [withinLimits row, withinLimits col]
  where
    withinLimits x = 0 <= x && x < _SIZE_

-- Q#09

stringToMove :: String -> Move
stringToMove (x:y:[]) =
  if (row >= 0 && col >= 0) then (row, col) else _INVALID_MOVE_
  where
    row = convertRowIndex x
    col = readDigit y
stringToMove _ = _INVALID_MOVE_

-- Q#10

replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow player col row
  | col < 0 || col >= length row = row
  | otherwise = updatedRow
  where
    (before, (_:after)) = splitAt col row
    updatedRow = concat [before, [player], after]

rsX :: Int -> Row -> Row
rsX = replaceSquareInRow X

rsO :: Int -> Row -> Row
rsO = replaceSquareInRow O

