module Checkers.BasicPrint (printState, printBoard) where

import Checkers.Types

{-
    Basic pretty-printing functionality.
-}
printState :: GameState -> [String] 
printState s = x:xs
    where x = show (status s)
          xs = printBoard s


printBoard :: GameState -> [String]
printBoard b = [printHLine y b | y <- [0..7]]

-- Display board
printHLine :: Int -> GameState -> String 
printHLine y b = [printSquare (x,y) b | x <- [0..8] ]

printSquare :: Coord -> GameState -> Char
printSquare xy b 
    | xy `elem` (redPieces b) = 'r'
    | xy `elem` (redKings b)  = 'R'
    | xy `elem` (blackPieces b) = 'b'
    | xy `elem` (blackKings b) = 'B' 
    | otherwise = ' '

