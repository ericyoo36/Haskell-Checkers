-- CPSC 449 Winter 2021
-- Assignment 1 Haskell Checkers Moves.hs
-- Seongmok, Yoo (Eric)
-- 10162624
-- references used CPSC 449 D2L, Assignment page https://pages.cpsc.ucalgary.ca/~robin/class/449/assignments/checker.html
-- template code used from Alex's tutorial

module Moves where

import Checkers.Types


-- moves function takes GameState and returns all the valid movements that player can make
moves :: GameState -> ([Move], [Move])
moves g = (simple_moves g, jump_moves g)

-- simple moves handle simple moves where Pawn or King move 1 block diagonaly into allowed direction
-- takes GameState and returns its possible simple moves
-- code from Alex's tutorial sections
simple_moves:: GameState -> [Move]
simple_moves st
                | _status st == Red
                    = (simpleKing (redKings st) st)++(simplePiece (redPieces st) st)
                | _status st == Black
                    = (simpleKing (blackKings st) st)++ (simplePiece (blackPieces st) st)
                | otherwise = []

-- simplePiece handle simple moves where pawn can make
-- pawns are only allowed to go foward until they reach the end of the board to become a king
-- takes current location of the pawn and GameState then return all possible simple Moves
-- code from Alex's tutorial sections
simplePiece :: [Coord] -> GameState -> [[PorK Coord]]
simplePiece xs st = [ [P (x,y), (if y' == 0 || y' == 7 then K else P) (x',y')]
                  | (x,y) <- xs
                  , (x',y') <- let y'=y+(dir st) in [(x+1,y'),(x-1,y')]
                  , notoccupied (x',y') st && onboard (x',y')]

-- simpleKing handle simple moves where king can make
-- kings are allowed to travel in any direction they wish
-- takes current location of the pawn and GameState then return all possible simple Moves
simpleKing :: [Coord] -> GameState -> [[PorK Coord]]
simpleKing xs st  = [ [K (x,y), K (x',y')]
                  | (x,y) <- xs
                  , (x',y') <- [(x+1,y+1),(x-1,y+1),(x+1,y-1),(x-1,y-1)]
                  , notoccupied (x',y') st && onboard (x',y') && noRepeat (history st) [K (x,y), K (x',y')] (x',y')]

-- jump_moves handle jump moves where pawn or king jumps over the opponent's pawn or kings
-- takes GameState and returns its possible jump Moves
-- code from Alex's tutorial sections
jump_moves:: GameState -> [Move]
jump_moves st
                | _status st == Red
                    = (jumpKing (redKings st) st)++(jumpPiece (redPieces st) st)
                | _status st == Black
                    = (jumpKing (blackKings st) st)++ (jumpPiece (blackPieces st) st)
                | otherwise = []

jump_over [] = [[]]
jump_over z = z

-- jumpPiece handle jump moves where pawn can make
-- pawns can only make a jump movement forward
-- takes location of other pieces to check if jump move is available
jumpPiece :: PieceState -> GameState -> [Move]
jumpPiece xs st = [P (x,y):ys | (x,y) <- xs, ys <- jumpPiece' (x,y) [] (x,y) st]

-- jump move is only possible when desired destination is not occupied by other pawns or king or enemys pawns or kings
-- also jump move has to me made with in the range of the board
-- if pawn reaches the end of the board after the jump, it becomes king
jumpPiece' :: Coord -> [Coord] -> Coord -> GameState -> [Move]
jumpPiece' start rem (x,y) st =
        [(if y'' == 0 || y'' == 7 then K else P) (x'',y''):ys | ((x',y'),(x'',y'')) <- [((x+1,y+(dir st)),(x+2,y+2*(dir st))),
                                                ((x-1,y+(dir st)),(x-2,y+2*(dir st)))],
                        not ((x',y') `elem` rem) && opponent_occupied (x',y') st &&
                        (notoccupied (x'', y'') st || start == (x'',y'')) && onboard (x'',y''),
                        ys <- case (is_king (x'',y'') (status st)) of
                                    True -> jump_over (jumpKing' start ((x',y'):rem) (x'',y'')st)
                                    False -> jump_over (jumpPiece' start ((x',y'):rem) (x'',y'')st) ]

-- jumpking handle jump moves where king can make
-- kings can jump in any direction that they wish
-- takes locations of other pieces to check if jump move is available
-- code from Alex's tutorial sections
jumpKing :: PieceState -> GameState -> [Move]
jumpKing xs st = [K (x, y):ys | (x, y) <- xs, ys <- jumpKing' (x, y) [] (x, y) st]

-- jump move is only possible when desired destination is not occupied by other pawns or king or enemys pawns or kings
-- also jump move has to me made with in the range of the board
jumpKing' :: Coord -> [Coord] -> Coord -> GameState -> [Move]
jumpKing' start rem (x,y) st =
        [K (x'',y''):ys | ((x',y'),(x'',y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2)),
                                                ((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))],
                        not (elem (x',y') rem) && opponent_occupied (x',y') st &&
                        (notoccupied (x'', y'') st || start == (x'',y'')) && onboard (x'',y''),
                        ys <- jump_over (jumpKing' start ((x',y'):rem) (x'',y'')st) ]

-- helper functions

-- checks if pawn has become the king
-- code from Alex's tutorial sections
is_king :: Coord -> Status -> Bool
is_king (_,y) st = case st of
  (Turn Black) -> y==7
  (Turn Red) -> y==0
  GameOver -> False

-- checks which player's turn it is
-- code from Alex's tutorial sections
_status :: GameState -> Player
_status st
    | status st == Turn Red = Red
    | otherwise = Black

-- checks direction of which original pieces can go
-- code from Alex's tutorial sections
dir :: GameState -> Int
dir g
    | _status g == Red = -1
    | otherwise = 1

-- checks if certain location of the board is occupied
-- code from Alex's tutorial sections
notoccupied :: (Int, Int) -> GameState -> Bool
notoccupied pos st
    | pos `elem` (redPieces st) = False
    | pos `elem` (blackPieces st) = False
    | pos `elem` (redKings st) = False
    | pos `elem` (blackKings st) = False
    | otherwise = True

-- checks if movement is in the board range
-- code from Alex's tutorial sections
onboard :: (Int, Int) -> Bool
onboard (x, y)
    | (x >= 0 && x <= 7 && y >= 0 && y <= 7) = True
    | otherwise = False

-- checks if certain location of the board is occupied with opponent pieces
opponent_occupied :: Coord -> GameState -> Bool
opponent_occupied xy st
                | _status st == Red
                    = xy `elem` blackPieces st || xy `elem` blackKings st
                | _status st == Black
                    = xy `elem` redPieces st || xy `elem` redKings st
                | otherwise = False

-- checks for the repeated states
-- takes a movement and history then trace back to see if certain piece was from same location which it is desired to travel to
noRepeat :: [Move] -> Move -> Coord -> Bool
noRepeat [] _ _ = True
noRepeat ([K a, K b]:ms) [K c, K d] x
    | b == c && a == x = False
    | b == c && a /= x = noRepeat ms [K a, K b] x
    | otherwise = noRepeat ms [K c, K d] x
noRepeat ([P a, P b]:ms) [K c, K d] x = noRepeat ms [K c, K d] x
noRepeat ([P a, K b]:ms) [K c, K d] x = noRepeat ms [K c, K d] x
