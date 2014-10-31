--
-- Tower of Hanoi - reason.hs
-- Implements game logic
--
-- Jonatan H Sundqvist
-- October 31 2014
--

-- TODO | -
--        -

-- SPEC | -
--        -



module Reason where



-------------------------------------------------------------------------
-- We'll need these
-------------------------------------------------------------------------
import Data.List (transpose, intersperse)



-------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------
data Board = Board [Int] [Int] [Int]
data Peg = First | Second | Third deriving (Show, Read)
--data Board = Board { First :: [Int], Second :: [Int], Third :: [Int] }


-- Retrieves the disk on the specified peg
choosePeg :: Board -> Peg -> [Int]
choosePeg (Board a _ _) First 	= a
choosePeg (Board _ b _) Second 	= b
choosePeg (Board _ _ c) Third 	= c


--
setPeg :: Peg -> [Int] -> Board -> Board
setPeg First disks (Board a b c) 	= Board disks b c
setPeg Second disks (Board a b c) 	= Board a disks c
setPeg Third disks (Board a b c) 	= Board a b disks


-- 
createGame :: Int -> Board
createGame n = Board [1..n] [] []


-- Moves a disk between two pegs
-- TODO - Refactor, clarify, comment
move :: Peg -> Peg -> Board -> Board
move frPeg toPeg board = setPeg toPeg (head from : to) . setPeg frPeg (tail from) $ board
  where
  	from = choosePeg board frPeg
  	to = choosePeg board toPeg


-- Moves a disk between two pegs if the 
-- TODO | Signal sucess, Maybe monad (?)
moveSafe :: Peg -> Peg -> Board -> Board
moveSafe frPeg toPeg board = if isValid frPeg toPeg board then move frPeg toPeg board else board


-- Checks if a particular move is valid
isValid :: Peg -> Peg -> Board -> Bool
isValid frPeg toPeg board = (not . null $ from) && ((null to) || (head from < head to))
  where
  	from = choosePeg board frPeg
  	to = choosePeg board toPeg


-- A board is completed when the first two pegs are empty and the third 
hasWon :: Board -> Bool
hasWon (Board a b c) = null a && null b && isSorted c
  where
  	isSorted xs = all (uncurry (<=)) $ zip xs (tail xs)


--
padLeft :: Int -> a -> [a] -> [a]
padLeft sz fill xs = replicate (sz - length xs) fill ++ xs


--
instance Show Board where
	show (Board a b c) = concat . map ((++"\n") . intersperse ' ') . transpose $ map (padLeft size '.' . concat . map show) [a,b,c]
		where
			--size = maximum . map length $ [a,b,c]
			size = maximum $ a ++ b ++ c



-------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------
--main = return ()