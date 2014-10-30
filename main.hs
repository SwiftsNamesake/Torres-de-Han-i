--
-- Tower of Hanoi - main.hs
-- Simple console implementation of the classical puzzle game
--
-- Jonatan H Sundqvist
-- October 28 2014
--

-- TODO | - More convenient representation (get rid of bloat)
--        - Variable number of pegs
--        - Solver, rules and instructions
--        - GHC build options
--        - Cabal and Haddock
--        - ASCII art comments and output
--        - 3D
--        - Save moves, undo feature
--        - Colours, console cursor
--        - Cheats
--        - Gamestate and options type (score, size, etc.)

-- SPEC | -
--        -




-------------------------------------------------------------------------
-- We'll need these
-------------------------------------------------------------------------
import Data.List (transpose, intersperse)
import Data.Char (ord)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode)
import System.Console.ANSI



-------------------------------------------------------------------------
-- Game logic (pure)
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
isValid frPeg toPeg board = (null to) || (head from < head to)
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
-- Interaction (impure)
-------------------------------------------------------------------------

-- TODO | readValue :: Read a => IO a
-- TODO | Handling invalid input, wrapping exceptions

-- Prompt the user to choose two pegs
-- TODO - Refactor with do notation (?)
-- TODO - Separate polymorphic prompt function (?)
askMove :: IO (Peg, Peg)
askMove = 	putStrF "From: " >>							-- Prompt user for first choice (from)
			getLine >>= 								-- Read the choice as a string
			(\fr -> putStrF "To: " >> return fr) >>= 	-- Prompt user for second choice (to)
			(\ fr -> getLine >>= (\ toStr -> return (read fr, read toStr) ))
			  where
			  	putStrF str = putStr str >> hFlush stdout -- Console is line buffered in interactive mode


--
doAskMove :: IO (Peg, Peg)
doAskMove = do
	putStr "From: "	-- Prompt user for first choice (from)
	fr <- getLine	-- Read the choice as a string
	putStr "To: "	-- Prompt user for second choice (to)
	to <- getLine	-- Read the choice as a string
	return (read fr, read to) -- 


--
-- TODO | recursive implementation in main (?)
-- TODO | Implement as pure function (strip away IO) (?)
-- or take IO actions as callbacks for greater flexibility
-- eg. run :: Board -> [(Peg, Peg)] -> Board
run :: Board -> IO ()
run board = do
	clearScreen
	setCursorPosition 0 0
	print board
	(fr, to) <- askMove
	let next = moveSafe fr to board in if not $ hasWon next then run next else clearScreen >> setCursorPosition 0 0 >> print next
	putStrLn "Hurrah, you've won!"

--run = return $ until hasWon (\ board -> print board >> askMove >>= (\ (fr, to) -> let (IO brd) = moveSafe fr to board in brd)) $ Board [1..5] [] []


--
runIOTests :: IO ()
runIOTests = do
	print $ createGame 5
	print $ Board [1..5] [] [1..3]
	print $ isValid First First (Board [1..5] [] [])
	print $ hasWon (Board [] [] [1..5])
	(fr, to) <- askMove
	print fr
	print to


--
runLogicTests :: IO ()
runLogicTests = do
	return ()



-------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------
main :: IO ()
main = run $ Board [1..5] [] []  -- Board [] [1] [2..5]