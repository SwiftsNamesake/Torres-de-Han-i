--
-- Tower of Hanoi - main.hs
-- Simple console implementation of the classical puzzle game
--
-- Jonatan H Sundqvist
-- October 28 2014
--

-- TODO | - More convenient representation (get rid of bloat)
--        -

-- SPEC | -
--        -


import Data.List (transpose, intersperse)


--data Board = Board { [Int] :: First, Second :: [Int], Third :: [Int]
data Board = Board [Int] [Int] [Int]
data Peg = First | Second | Third deriving (Show, Read)


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
-- TODO - Refactoring, clarify, comment
move :: Peg -> Peg -> Board -> Board
move pFrom pTo board = setPeg pTo (head from : to) . setPeg pFrom (tail from) $ board
--move board pFrom pTo = setPeg (setPeg board pTo (head from : to)) pFrom (tail from)
  where
  	from = choosePeg board pFrom
  	to = choosePeg board pTo


-- Checks if a particular move is valid
isValid :: Board -> Peg -> Peg -> Bool
isValid board pFrom pTo = (null to) || (head from < head to)
  where
  	from = choosePeg board pFrom
  	to = choosePeg board pTo



-- A board is completed when the first two pegs are empty and the third 
hasWon :: Board -> Bool
hasWon (Board a b c) = null a && null b and isSorted c
  where
  	isSorted xs = all (uncurry (<=)) $ zip xs (tail xs)


instance Show Board where
	--show (Board a b c) = concat $ zipWith3 (\ a b c -> intersperse ' ' $ a:b:c:"\n") (concat . map show $ a) (concat . map show $ b) (concat . map show $ c)
	show (Board a b c) = concat . map ((++"\n") . intersperse ' ') . transpose $ map (concat . map show) [a,b,c]


main :: IO ()
main = do
	putStrLn "Hello World"
	--print $ createGame 5
	print $ Board [1..5] [1..5] [1..5]
	print $ isValid (Board [1..5] [1..5] [1..5]) First First