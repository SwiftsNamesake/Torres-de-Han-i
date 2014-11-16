--
-- Tower of Hanoi - main.hs
-- Simple console implementation of the classical puzzle game
--
-- Jonatan H Sundqvist
-- October 28 2014
--

-- TODO | - Define interface
--        - More convenient representation (get rid of bloat)
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
--        - BUG: Several victory messages are printed
--        - BUG: Invalid moves lead to crashes (isValid) (✓)
--        - Separate polymorphic prompt function (?)
--        - Unified drawGame action (board, score, etc.)

-- SPEC | -
--        -



-------------------------------------------------------------------------
-- We'll need these
-------------------------------------------------------------------------
import Reason
import Data.List (transpose, intersperse)
import Data.Char (ord)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode)
import System.Console.ANSI



-------------------------------------------------------------------------
-- Game logic (pure)
-------------------------------------------------------------------------



-------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------

-- Validates move input
validInput :: String -> Bool
validInput = flip elem ["First", "Second", "Third"]


-- Assuming p, do action A, else do action B (ternary operator as function)
-- TODO | Rename (?)
assuming :: Bool -> a -> a -> a
assuming p a b = if p then a else b


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
			(\fr -> getLine >>= (\ toStr -> return (read fr, read toStr) ))
			  where
			  	putStrF str = putStr str >> hFlush stdout -- Console is line buffered in interactive mode


--
doAskMove :: IO (Peg, Peg)
doAskMove = do
	--putStr "From: "	-- Prompt user for first choice (from)
	--fr <- getLine	-- Read the choice as a string
	--putStr "To: "	-- Prompt user for second choice (to)
	--to <- getLine	-- Read the choice as a string
	fr <- safeInput "From: "
	to <- safeInput "To: "
	return (fr, to) -- 


--	
-- TODO | Generic prompt version
-- TODO | See if Read instances implement validation function
-- TODO | Error message (see askUntil in Python)
safeInput :: String -> IO Peg
safeInput prompt = do
	putStr prompt
	hFlush stdout
	input <- getLine
	if validInput input
		then return $ read input
		else askAgain
	where askAgain = do
			setCursorColumn 0
			cursorUpLine 1
			clearLine
			safeInput prompt


--
-- TODO | Recursive implementation in main (?)
-- TODO | Implement as pure function (strip away IO) (?)
-- or take IO actions as callbacks for greater flexibility
-- eg. run :: Board -> [(Peg, Peg)] -> Board
run :: Board -> IO ()
run board = do
	clearScreen
	setCursorPosition 0 0
	print board
	(fr, to) <- doAskMove
	let next = moveSafe fr to board in if not $ hasWon next then run next else do
		clearScreen
		setCursorPosition 0 0
		print next
		putStrLn "Hurrah, you've won!"

--run = return $ until hasWon (\ board -> print board >> askMove >>= (\ (fr, to) -> let (IO brd) = moveSafe fr to board in brd)) $ Board [1..5] [] []




-------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------
main :: IO ()
main = run $ Board [] [1..2] [3..5]  -- Board [] [1] [2..5]