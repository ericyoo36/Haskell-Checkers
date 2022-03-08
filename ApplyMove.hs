-- CPSC 449 Winter 2021
-- Assignment 1 Haskell Checkers ApplyMove.hs
-- Seongmok, Yoo (Eric)
-- 10162624
-- references used CPSC 449 D2L, Assignment page https://pages.cpsc.ucalgary.ca/~robin/class/449/assignments/checker.html


module ApplyMove where

import Moves
import Checkers.Types


-- apply move checks if desired move is a valid movement
-- if it is, apply the move
-- if no movement is available set the GameOver
-- takes a move and GameState and return updated GameState
apply_move :: Move -> GameState -> GameState
apply_move m st
    | (moves st) == ([],[]) = st{status = GameOver}
    | inmoves m (moves st) == 1 = make_simple_move (unwrap m) st
    | inmoves m (moves st) == 0 = make_jump_move (unwrap m) (st{history = m:(history st)})
    | otherwise = st{message = "Illegal move!!"}

inmoves :: Move -> ([Move], [Move]) -> Int
inmoves m (a, b)
    | elem m a = 1
    | elem m b = 0
    | otherwise = -1

-- generate PieceState from moves
unwrap :: [PorK Coord] -> [Coord]
unwrap  [] = []
unwrap (x:xs) = (removepk x)++(unwrap xs)

removepk :: PorK a -> [a]
removepk (P x) = [x]
removepk (K x) = [x]

-- make_simple_move hands simple moves and updates GameState accordingly
-- takes PieceState and GameState then returns updated GameState
make_simple_move :: PieceState -> GameState -> GameState
make_simple_move [start,end] st
    | status st == (Turn Black) && elem start (blackKings st)
        = st{blackKings = replace start end (blackKings st)
            , status = change_player st
            , message = "", history = [K start, K end]:(history st)}
    | status st == (Turn Black) && elem start (blackPieces st) && isking end (Turn Black)
        = st{blackPieces = remove start (blackPieces st)
            , blackKings = end:(blackKings st)
            , status = change_player st
            , message = "", history = [P start, K end]:(history st)}
    | status st == (Turn Black) && elem start (blackPieces st) && not(isking end (Turn Black))
        = st{blackPieces = replace start end (blackPieces st)
            , status = change_player st
            , message = "", history = [P start, P end]:(history st)}
    | status st == (Turn Red) && elem start (redKings st)
        = st{redKings = replace start end (redKings st)
            , status = change_player st
            , message = "", history = [K start, K end]:(history st)}
    | status st == (Turn Red) && elem start (redPieces st) && isking end (Turn Red)
        = st{redPieces = remove start (redPieces st)
            , redKings = end:(redKings st)
            , status = change_player st
            , message = "", history = [P start, K end]:(history st)}
    | status st == (Turn Red) && elem start (redPieces st) && not(isking end (Turn Red))
        = st{redPieces = replace start end (redPieces st)
            , status = change_player st
            , message = "", history = [P start, P end]:(history st)}
    | otherwise = st{message = "invalid make_simple_move"}

-- make_jump_move hands jump moves and updates GameState accordingly
-- takes PieceState and GameState then returns updated GameState
make_jump_move :: PieceState -> GameState -> GameState
make_jump_move (start:(next:[])) st   -- final iteration of jump moves
    | status st == (Turn Black) && elem start (blackKings st)
        = (st{redKings = remove (jumped start next) (redKings st)
                    , redPieces = remove (jumped start next) (redPieces st)
                    , blackKings = next:(remove start (blackKings st))
                    , status = Turn Red, message = ""})
    | status st == (Turn Black) && elem start (blackPieces st) && isking next (Turn Black)
        = (st{redKings = remove (jumped start next) (redKings st)
                    , redPieces = remove (jumped start next) (redPieces st)
                    , blackKings = next:(blackKings st)
                    , blackPieces = remove start (blackPieces st)
                    , status = Turn Red, message = ""})
    | status st == (Turn Black) && elem start (blackPieces st) && not(isking next (Turn Black))
        = (st{redKings = remove (jumped start next) (redKings st)
                    , redPieces = remove (jumped start next) (redPieces st)
                    , blackPieces = next:(remove start (blackPieces st))
                    , status = Turn Red, message = ""})
    | status st == (Turn Red) && elem start (redKings st)
        = (st{blackKings = remove (jumped start next) (blackKings st)
                    , blackPieces = remove (jumped start next) (blackPieces st)
                    , redKings = next:(remove start (redKings st))
                    , status = Turn Black, message = ""})
    | status st == (Turn Red) && elem start (redPieces st) && isking next (Turn Red)
        = (st{blackKings = remove (jumped start next) (blackKings st)
                    , blackPieces = remove (jumped start next) (blackPieces st)
                    , redKings = next:(redKings st)
                    , redPieces = remove start (redPieces st)
                    , status = Turn Black, message = ""})
    | status st == (Turn Red) && elem start (redPieces st) && not(isking next (Turn Red))
        = (st{blackKings = remove (jumped start next) (blackKings st)
                    , blackPieces = remove (jumped start next) (blackPieces st)
                    , redPieces = next:(remove start (redPieces st))
                    , status = Turn Black, message = ""})
make_jump_move (start:(next:rest)) st       -- recursive call of jump_moves since multiple jump is possible
    | status st == (Turn Black) && elem start (blackKings st)
        = make_jump_move (next:rest)
                (st{redKings = remove (jumped start next) (redKings st)
                    , redPieces = remove (jumped start next) (redPieces st)
                    , blackKings = next:(remove start (blackKings st))
                    , message = ""})
    | status st == (Turn Black) && elem start (blackPieces st) && isking next (Turn Black)
        = make_jump_move (next:rest)
                (st{redKings = remove (jumped start next) (redKings st)
                    , redPieces = remove (jumped start next) (redPieces st)
                    , blackKings = next:(blackKings st)
                    , blackPieces = remove start (blackPieces st)
                    , message = ""})
    | status st == (Turn Black) && elem start (blackPieces st) && not(isking next (Turn Black))
        = make_jump_move (next:rest)
                (st{redKings = remove (jumped start next) (redKings st)
                    , redPieces = remove (jumped start next) (redPieces st)
                    , blackPieces = next:(remove start (blackPieces st))
                    , message = ""})
    | status st == (Turn Red) && elem start (redKings st)
        = make_jump_move (next:rest)
                (st{blackKings = remove (jumped start next) (blackKings st)
                    , blackPieces = remove (jumped start next) (blackPieces st)
                    , redKings = next:(remove start (redKings st))
                    , message = ""})
    | status st == (Turn Red) && elem start (redPieces st) && isking next (Turn Red)
        = make_jump_move (next:rest)
                (st{blackKings = remove (jumped start next) (blackKings st)
                    , blackPieces = remove (jumped start next) (blackPieces st)
                    , redKings = next:(redKings st)
                    , redPieces = remove start (redPieces st)
                    , message = ""})
    | status st == (Turn Red) && elem start (redPieces st) && not(isking next (Turn Red))
        = make_jump_move (next:rest)
                (st{blackKings = remove (jumped start next) (blackKings st)
                    , blackPieces = remove (jumped start next) (blackPieces st)
                    , redPieces = next:(remove start (redPieces st))
                    , message = ""})
    | otherwise = st{message = "invalid make_simple_move"}

-- helper functions

-- checks if pawn has become the king
isking :: Coord -> Status -> Bool
isking (_,y) st = case st of
    (Turn Black) -> y==7
    (Turn Red) -> y==0
    GameOver -> False

-- replace location of the pieces
replace :: Coord -> Coord -> PieceState -> PieceState
replace x z ys = z:(remove x ys)

-- return the coordinate of the enemy's piece to be deleted
jumped :: Coord -> Coord -> Coord
jumped (x,y) (a,b)
    | x > a && y > b = (a+1, b+1)
    | x > a && y < b = (a+1, y+1)
    | x < a && y > b = (x+1, b+1)
    | x < a && y < b = (x+1, y+1)

-- remove piece from the PieceState
remove :: Coord -> PieceState -> PieceState
remove x ys = [y | y <- ys, y /= x]

-- change turns of the game
change_player :: GameState -> Status
change_player st
    | status st == (Turn Red)
        = (Turn Black)
    | status st == (Turn Black)
        = (Turn Red)
