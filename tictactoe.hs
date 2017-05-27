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
import System.Random
import System.Console.ANSI
import System.IO.Unsafe
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
	--putStr s
	line <- getLine
	return (read line :: Int)

opponent :: Player -> Player
opponent X = O
opponent O = X

----------------- MINIMAX
data MTree = Nil
           | Node Board Int [MTree] deriving (Show, Ord, Eq)

posList :: [(Int, Int)]
posList = [(x,y) | x <- [1..3], y <- [1..3]]


getScoreRnd a b = unsafePerformIO $ randomRIO (a,b :: Int)
--scorify :: Num t => Maybe Board -> t
scorify board
    | isNothing (whoWins2 board) = 0
    | whoWins2 board == Just X = -10
    | whoWins2 board == Just D = 0
    | whoWins2 board == Just O = 10

getBoards :: Board -> [(Int, Int)] -> Player -> [Maybe Board]
getBoards _ [] _ = []
getBoards b (p:ps) pl
    | isNothing (setBoard pl p (Just b)) = getBoards b ps pl
    | otherwise = (setBoard pl p (Just b)):getBoards b ps pl

populateMTL :: Board -> Player -> Int -> [MTree]
populateMTL b pl lvl = map (\s -> createTree2 (fromJust s) (pl) lvl) (getBoards b posList (pl))

adjustLocalScore pl sl
	| pl == O = maximum sl
	| pl == X = minimum sl

calcScoreBelow :: Board -> Player -> Int -> Int
calcScoreBelow b pl lvl = lvl + foldl (+) 0 (map (\(Node _ i _) -> i) (populateMTL b (opponent pl) (lvl+1)))

calcScoreBelow2 :: Board -> Player -> Int -> Int
calcScoreBelow2 b pl lvl = lvl + adjustLocalScore (opponent pl) (map (\(Node _ i _) -> i) (populateMTL b (opponent pl) (lvl+1)))

createTree2 :: Board -> Player -> Int -> MTree
createTree2 b pl lvl
    | isNothing (whoWins2 $ Just b) =
		(Node b (calcScoreBelow2 b pl lvl) (populateMTL b (opponent pl) (lvl+1)))
    | otherwise = (Node b (lvl + (scorify (Just b))) [Nil])

createTreeMonadic :: Board -> Player -> Int -> MTree
createTreeMonadic b pl lvl = do
	let subTree = populateMTL b (opponent pl) (lvl+1)
	if isNothing (whoWins2 (Just b)) then
		(Node b (lvl + (foldl (+) 0 (map (\(Node _ i _ ) -> i) subTree)))) subTree
	else
		(Node b (lvl + (scorify (Just b))) [Nil])

getBestMoveList2 :: MTree -> [(Board, Int)]
getBestMoveList2 (Node _ _ n) = onlyTheBest (getBestMoveList2Helper n) ((\(_,x)->x) $ (maximumBy (comparing snd) (getBestMoveList2Helper n)))
	where
		getBestMoveList2Helper [] = []
		getBestMoveList2Helper ((Node b i _):ms) = (b, i):getBestMoveList2Helper ms
		onlyTheBest [] _ = []
		onlyTheBest ((b,i):bs) mx
			| i < mx = onlyTheBest bs mx
			| otherwise = (b,i):onlyTheBest bs mx

getBestMove :: MTree -> (Board, Int)
getBestMove mt = (getBestMoveList2 mt) !! (getScoreRnd 0 ((length (getBestMoveList2 mt))-1))

getTheBoard :: (a, t) -> Maybe a
getTheBoard (b, i) = Just b

clear :: IO ()
clear = putStr "\ESC[2J"

----------------- MAIN FUNCTIONS

gamify :: Maybe Board -> Player -> IO ()
gamify board p = do
	let state = whoWins2 board
	clearScreen
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

gamify2 :: Maybe Board -> Player -> IO ()
gamify2 board p = do
	let state = whoWins2 board
	clearScreen
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
					--putStrLn (show (getBestMove $ createTreeMonadic (fromJust board) X 0))
					let bestBoard = getTheBoard $ getBestMove $ createTree2 (fromJust board) X 0
					gamify2 bestBoard (opponent p)
			| s == Just D = do {putStrLn "Draw!"; rematch (p)}
			| (whoWins2 board == Just O) = do {putStrLn "The machine shall reign over you!\n"; rematch (p)}
			| otherwise = do {putStrLn $ "Congratulations! You beat the machine."; rematch (p)}

rematch p = do {putStrLn "Want a rematch? [y/n]"; opt <- getLine; case opt of
				['y'] -> gamify2 emptyBoard p
				otherwise -> exitSuccess}

main :: IO()
main = do
	args <- getArgs
	case args of
		["-p"] -> gamify emptyBoard X
		["-X"] -> gamify2 emptyBoard X
		["-O"] -> gamify2 emptyBoard O
