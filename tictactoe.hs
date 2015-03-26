module TicTacToe where

import Data.List
import System.IO
import Text.Read
import Control.Monad
import System.Random




tictactoe :: IO()
tictactoe = do
	player <- randomRIO (0,1)::IO Integer
	f player $ replicate 9 "free" where
		f 0 xs = do			--KI
			putStrLn "KI move"
			move <- randomRIO (0,8)
			if xs !! move == "free" then do
				let newField = take move xs ++ ["O"] ++ drop (1+move) xs
				if win "O" newField then putStrLn "KI wins" else f 1 newField
			else f 0 xs
		f 1 xs = do
			showField xs
			putStrLn "Player move:"
			move <- getInt
			if move >= 0 && move <=8 then 
				if xs !! move == "free" then do
					let newField = take move xs ++ ["X"] ++ drop (1+move) xs
					if win "X" newField then putStrLn "Player wins" else f 0 newField
				else putStrLn "invalid move" >> f 1 xs
			else putStrLn "invalid number" >> f 1 xs
		showField [] = putStrLn ""
		showField xs = putStrLn (foldr (\a b -> a ++ "\t" ++ b)"" $ take 3 xs) >> showField (drop 3 xs) 
		getInt = do
			ln <- getLine
			case (readMaybe ln ::Maybe Int) of
				Just x -> return x
				_		-> putStrLn "invalid" >> getInt
		win c xs = checkLines xs || checkRows 0 xs || checkDiag xs where
			checkLines [] = False
			checkLines zs = (foldl (\b a -> b && a == c) True (take 3 zs))|| checkLines (drop 3 zs)
			checkRows x xs = if x > 2 then False else
				((xs !! x) == c && c ==(xs !! (x+3)) && c == (xs !! (x+6))) || checkRows (x+1) xs
			checkDiag xs = ((xs !! 0) == c && c == (xs !! 4) && c == (xs !! 8)) || ((xs !! 2) == c && c == (xs !! 4) && c == (xs !! 6))
			
			