module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2

-- Q#01

showInts :: [Int] -> [String]
showInts [] = []
showInts (x : xs) = show x : showInts xs

_HEADER_ :: String
_HEADER_ =
  concat
    [ " ",
      formatLine . showInts $ _RANGE_,
      " "
    ]

-- Q#02

showSquares :: [Square] -> [String]
showSquares [] = []
showSquares (x : xs) = showSquare x : showSquares xs

-- Q#03

formatRows :: [Row] -> [String]
formatRows [] = []
formatRows (x : xs) = formatRow x : formatRows xs
  where
    formatRow = formatLine . showSquares

-- Q#04

isColEmpty :: Row -> Int -> Bool
isColEmpty [] _ = False
isColEmpty (x : xs) 0 = x == E
isColEmpty (x : xs) n = isColEmpty xs (n - 1)

-- Q#05

dropFirstCol :: Board -> Board
dropFirstCol xs = dropFirstCol' <$> xs
  where
    dropFirstCol' :: Row -> Row
    dropFirstCol' [] = []
    dropFirstCol' (x : xs) = xs

dropLastCol :: Board -> Board
dropLastCol xs = dropLastCol' <$> xs
  where
    dropLastCol' :: Row -> Row
    dropLastCol' [] = []
    dropLastCol' [x] = []
    dropLastCol' (x : xs) = x : dropLastCol' xs

-- Q#06

getDiag1 :: Board -> Line
getDiag1 [] = []
getDiag1 (x : xs) = getDiag1Safe x xs
  where
    getDiag1Safe :: Line -> Board -> Line
    getDiag1Safe [] _ = []
    getDiag1Safe x xs = head x : (getDiag1 . dropFirstCol $ xs)

getDiag2 :: Board -> Line
getDiag2 = getDiag1 . reverse

getAllLines :: Board -> [Line]
getAllLines xs =
  concat
    [ xs,
      transpose xs,
      [getDiag1 xs, getDiag2 xs]
    ]

-- Q#07

putSquare :: Player -> Board -> Move -> Board
putSquare p b m = putSquare' b m
  where
    putSquare' :: Board -> Move -> Board
    putSquare' [] _ = []
    putSquare' (r : rx) (0, y) = replaceSquareInRow p y r : rx
    putSquare' (r : rx) (x, y) = r : putSquare' rx (x - 1, y)

-- Q#08

prependRowIndices :: [String] -> [String]
prependRowIndices ss = prepend pairs
  where
    pairs :: [(Char, String)]
    pairs = indexRowStrings ss
    prepend :: [(Char, String)] -> [String]
    prepend [] = []
    prepend ((c, s) : px) = (c : s) : prepend px

-- Q#09

isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ p = isWinningLine' False
  where
    isWinningLine' :: Bool -> Line -> Bool
    isWinningLine' acc [] = acc
    isWinningLine' acc (x : xs)
      | x == p = isWinningLine' True xs
      | otherwise = False

-- Q#10

isValidMove :: Board -> Move -> Bool
isValidMove b m
  | not . isMoveInBounds $ m = False
  | otherwise = isValidMove' b m
  where
    isValidMove' :: Board -> Move -> Bool
    isValidMove' [] _ = False
    isValidMove' (r : rx) (0, y) = isColEmpty r y
    isValidMove' (r : rx) (x, y) = isValidMove' rx (x - 1, y)
