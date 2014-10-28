--
-- Tower of Hanoi - main.hs
-- Simple console implementation of the classical puzzle game
--
-- Jonatan H Sundqvist
-- October 28 2014
--

-- TODO | -
--        -

-- SPEC | -
--        -



data Board = Board [Int] [Int] [Int]
data Peg = First | Second | Third deriving (Show, Read)


--
choosePeg :: Board -> Peg -> [Int]
choosePeg (Board a _ _) First 	= a
choosePeg (Board _ b _) Second 	= b
choosePeg (Board _ _ c) Third 	= c


-- 
createGame :: Int -> Board
createGame n = Board [1..n] [] []


-- Moves a disk between two pegs
move :: Board -> Peg -> Peg -> Board
move board from to = board


-- Checks if a particular move is valid
isValid :: Board -> Peg -> Peg -> Bool
isValid board pFrom pTo = (null to) || (head from < head to)
  where
  	from = choosePeg board pFrom
  	to = choosePeg board pTo



instance Show Board where
	show (Board a b c) = ""


main :: IO ()
main = do
	putStrLn "Hello World"
	print $ createGame 5
