import System.IO
import System.Environment
import System.Exit
import Control.Monad
import Data.Ord
import Data.List
import Data.List.Split
import Data.Word
import Data.Char
import Data.Maybe
import qualified Data.Matrix as M
import Numeric (showHex, showIntAtBase)

data Player = O | X | V deriving (Eq, Show, Ord)
type Board = [[Player]] 

-- whoWins2 :: Board -> Maybe Player
whoWins2 board = listToMaybe $ filter (/=V) [lin board, lin (transpose board), diag board]
    where
        lin [] = V
        lin (b@[a,_,_]:bs) = if (all (==a) b && a /= V) then a else lin bs
        diag [[x1,x2,x3],[y1,y2,y3],[z1,z2,z3]]
            | (x1 == y2 && y2 == z3 && x1 /= V) = x1
            | (x3 == y2 && y2 == z1 && x3 /= V) = x3
            | otherwise = V

putBoard :: Board -> IO()
putBoard b = putStrLn . M.prettyMatrix $ M.fromLists b

setBoard :: Player -> (Int, Int) -> Board -> Board
setBoard v (x,y) b = M.toLists $ M.setElem v (x,y) $ M.fromLists b

emptyBoard = [ [V, V, V],
               [V, V, V],
               [V, V, V] ]

prompt s = do
	putStr s
	line <- getLine
	return (read line :: Int)

opponent X = O
opponent O = X

gamify board p = do
	let state = whoWins2 board
	putBoard board
	if (isNothing state) then
		do
			putStrLn ("Player " ++ show p ++ " turn.")
			x <- prompt "line: "
			y <- prompt "column: "
			putStrLn ""
			gamify (setBoard p (x,y) board) (opponent p)
		else putStrLn $ "Player " ++ show (fromJust state) ++ " wins!"

