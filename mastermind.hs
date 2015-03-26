module MasterMind where

import Data.List
import System.IO
import System.Random
import Text.Read
import Control.Monad



masterMind :: IO()
masterMind =  randomNumbers 4 >>= game 0 where
	game round numbers = if round >= 15 then lost round numbers else do
		putStrLn $ show round ++ ". Versuch:"
		ln <- getLine >>= return . words
		if length ln /= 4 then invalid round numbers
		else case sequence (map (\x->validNumber (readMaybe x :: Maybe Integer)) ln) of
			Nothing -> invalid round numbers
			Just xs -> if checkWin xs numbers then putStrLn "Player wins" 
					else putStrLn (show (checkPos xs numbers)) >> game (round+1)numbers
		where
			checkPos zs numbers = foldl (\(b,c) a -> if a == 0 then (b,c)else if a == 1 then (b, c+1) else (b+1,c)) (0,0)$
				map (\x->if zs !! x `elem` numbers then if zs !! x == numbers !! x then 1 else 2 else 0)[0..3]
			checkWin (y:ys)(z:zs) = z == y && checkWin ys zs 
			checkWin _ _ = True
			validNumber (Just x) = if x `elem` [0..9] then Just x else Nothing
			validNumber _ = Nothing
	randomNumbers 0 = return []
	randomNumbers n = (randomRIO (0,9):: IO Integer) >>= \x -> randomNumbers (n-1) >>= \xs -> return $ x:xs
	invalid round numbers = putStrLn "invalid input, the input must be like 1 2 3 4" >> game round numbers
	lost round numbers = do 
		putStrLn $ "you lost the game after " ++ show round ++ " rounds"
		putStrLn $ "the combination was " ++ show numbers 