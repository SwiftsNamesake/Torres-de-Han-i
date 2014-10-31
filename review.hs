--
-- Tower of Hanoi - review.hs
-- UI and logic tests
--
-- Jonatan H Sundqvist
-- October 31 2014
--

-- TODO | -
--        -

-- SPEC | -
--        -



module Review where



-------------------------------------------------------------------------
-- We'll need these
-------------------------------------------------------------------------
import Reason



-------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------

--
runIOTests :: IO ()
runIOTests = do
	print $ createGame 5
	print $ Board [1..5] [] [1..3]
	print $ isValid First First (Board [1..5] [] [])
	print $ hasWon (Board [] [] [1..5])
	(fr, to) <- doAskMove
	print fr
	print to


--
runLogicTests :: IO ()
runLogicTests = do
	return ()



-------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------
main = do
	runLogicTests
	runIOTests