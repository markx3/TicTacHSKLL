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

data Player = O | X | V | D deriving (Eq, Ord)
type Board = [[Player]] 

instance Show Player where
	show O = "O"
	show X = "X"
	show V = " "
	show D = "D"

whoWins2 :: Maybe Board -> Maybe Player
whoWins2 board = listToMaybe $ filter (/=V) [lin $ fromJust board, lin (transpose $ fromJust board), diag $ fromJust board]
    where
        lin [] = V
        lin (b@[a,_,_]:bs) = if (all (==a) b && a /= V) then a else lin bs
        diag b@[[x1,x2,x3],[y1,y2,y3],[z1,z2,z3]]
            | (x1 == y2 && y2 == z3 && x1 /= V) = x1
            | (x3 == y2 && y2 == z1 && x3 /= V) = x3
            | all (==True) (map (all (/=V)) b) = D
            | otherwise = V

putBoard :: Maybe Board -> IO()
putBoard b = putStrLn . M.prettyMatrix $ M.fromLists $ fromJust b

setBoard :: Player -> (Int, Int) -> Maybe Board -> Maybe Board
setBoard v (x,y) b = setBoardHelper v (x,y) $ M.fromLists $ fromJust b where
	setBoardHelper v (x,y) b
	    | M.getElem x y b /= V = Nothing
	    | all (==True) (map (all (/=V)) (M.toLists b)) = Nothing
	    | otherwise = Just (M.toLists $ M.setElem v (x,y) b)

emptyBoard :: Maybe Board
emptyBoard = Just ( [ [V, V, V],
                      [V, V, V],
                      [V, V, V] ] )

prompt :: String -> IO Int
prompt s = do
	putStr s
	line <- getLine
	return (read line :: Int)

opponent :: Player -> Player
opponent X = O
opponent O = X

gamify :: Maybe Board -> Player -> IO ()
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
			    	if (isNothing (setBoard p (x,y) board)) then gamify board (opponent p) else
			    	    gamify (setBoard p (x,y) board) (opponent p)
			| s == Just D = putStrLn "Draw!"
			| otherwise = putStrLn $ "Player " ++ show (fromJust s) ++ " wins!"

------------- MINIMAX
data MTree = Nil
           | Node Board Int [MTree] deriving (Show, Ord, Eq)

getScore :: [MTree] -> Int
getScore [(Node _ i _)] = i
getScore [] = 0
getScore (x:xs) = getScore [x] + getScore xs

compareM l r = compare (getScore l) (getScore r)

posList :: [(Int, Int)]
posList = [(x,y) | x <- [1..3], y <- [1..3]]


scorify' b = foldl (+) 0 (map scorify b)

scorify board
    | isNothing (whoWins2 board) = 0
    | whoWins2 board == Just X = -10
    | whoWins2 board == Just O = 10

getBoards _ [] _ = []
getBoards b (p:ps) pl 
    | isNothing (setBoard pl p (Just b)) = getBoards b ps pl
    | otherwise = (setBoard pl p (Just b)):getBoards b ps pl

populateMTL b pl = map (\s -> createTree2 (fromJust s) (opponent pl)) (getBoards b posList (opponent pl))

createTree2 :: Board -> Player -> MTree
createTree2 b pl 
    | isNothing (whoWins2 (Just b)) = (Node b (foldl (+) 0 (map (\(Node _ i _) -> i) (populateMTL b (pl)))) (populateMTL b (pl)))
    | otherwise = (Node b (scorify' (getBoards b posList (opponent pl))) [Nil])

getBestMoveList2 (Node _ _ n) = getBestMoveList2Helper n where
	getBestMoveList2Helper [] = []
	getBestMoveList2Helper ((Node b i _):ms) = (b, i):getBestMoveList2Helper ms

getBestMove mt = (maximumBy (comparing snd) $ getBestMoveList2 mt)

getTheBoard (b, i) = Just b

gamify2 :: Maybe Board -> Player -> IO ()
gamify2 board p = do
	let state = whoWins2 board
	putBoard board
	playIt state
	where
		playIt s
		    | ((isNothing s) && (p == X)) = 
		    	do
		    		putStrLn ("Player " ++ show p ++ " turn.")
			    	x <- prompt "line: "
			    	y <- prompt "column: "
			    	putStrLn ""
			    	if (isNothing (setBoard p (x,y) board)) then gamify2 board (opponent p) else
			    	    gamify2 (setBoard p (x,y) board) (opponent p)
			| ((isNothing s) && (p == O)) =
				do
					let bestBoard = getTheBoard $ getBestMove $ createTree2 (fromJust board) X
					gamify2 bestBoard (opponent p)
			| s == Just D = putStrLn "Draw!"
			| otherwise = putStrLn $ "Player " ++ show (fromJust s) ++ " wins!"

main :: IO()
main = do
	args <- getArgs
	case args of
		["-p"] -> gamify emptyBoard X
		["-a"] -> gamify2 emptyBoard X

