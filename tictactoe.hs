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

whoWins2 :: Board -> Maybe Player
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
setBoard v (x,y) b = setBoardHelper v (x,y) $ M.fromLists b where
	setBoardHelper v (x,y) b
	    | M.getElem x y b /= V = M.toLists b
	    | 
	    | otherwise = M.toLists $ M.setElem v (x,y) b

emptyBoard :: Board
emptyBoard = [ [V, V, V],
               [V, V, V],
               [V, V, V] ]

prompt :: String -> IO Int
prompt s = do
	putStr s
	line <- getLine
	return (read line :: Int)

opponent :: Player -> Player
opponent X = O
opponent O = X

gamify :: Board -> Player -> IO ()
gamify board p = do
	let state = whoWins2 board
	putBoard board
	playIt state
	where
		playIt s
		    | isNothing s = 
		    	do
		    		putStrLn ("Player " ++ show p ++ " turn.")
			    	x <- prompt "line: "
			    	y <- prompt "column: "
			    	putStrLn ""
			    	gamify (setBoard p (x,y) board) (opponent p)
			| otherwise = putStrLn $ "Player " ++ show (fromJust s) ++ " wins!"

------------- MINIMAX
data MTree = Nil
           | Node Board Int [MTree] deriving (Show, Ord, Eq)

getScore (Node _ i _) = i
--getScore (Nil _ i) = i

compareM l r = compare (getScore l) (getScore r)

posList :: [(Int, Int)]
posList = [(x,y) | x <- [1..3], y <- [1..3]]

scorify board
    | isNothing (whoWins2 board) = 0
    | whoWins2 board == Just X = -10
    | otherwise = 10

--guessPos :: Board -> [(Int, Int)] -> [Board]
getBoards _ [] _ = []
getBoards b (p:ps) pl = (setBoard pl p b):getBoards b ps pl

guessPos _ [] _ = []
guessPos board (p:ps) player = (Node (setBoard player p board) (scorify $ setBoard player p board) ([predict board player])):guessPos board ps player

--guessPos2 [] _ = []
guessPos2 :: [Board] -> Player -> [MTree]
guessPos2 [] _ = []
guessPos2 (b:bs) pl = (Node b (foldl (+) 0 (map scorify (getBoards b posList pl))) (guessPos2 (getBoards b posList pl) pl ):guessPos2 bs pl

--predict :: Board -> Player -> MTree
predict board player = Node board (foldl (+) 0 (map getScore (guessPos board posList player))) (guessPos board posList player)

--traverseP (Node b i []) = ([Node ()])
--traverseP (Node b i nodes) = map traverseP nodes



traverseP [] _ = []
traverseP (m:ms) player = traverseP' m player ++ traverseP ms player where
	traverseP' m p = traverseAndPredict m p
traverseAndPredict (Node b i []) player = [predict b player]
traverseAndPredict (Node b i (m:ms)) player = traverseP [m] player ++ traverseP ms player
--traverseAndPredict (m:ms) player = 

--tAndP [] _ = []
--tAndP (Node b i []

--traverseAndWin root = 

--constructTree boards@(b:bs) = 


--minimaxify board = minimaxify . sortBy (compareM) $ (Node (foldl (+) 0 (getScore b)) )

	--	          Node (Val)
   --NOde B1 Node B2 (Node B3) (Node B4) (Node B5)

