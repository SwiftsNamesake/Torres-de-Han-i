--
-- Tower of Hanoi - render.hs
-- Graphics utilities
--
-- Jonatan H Sundqvist
-- October 29 2014
--

-- TODO | -
--        -

-- SPEC | -
--        -



module Render where



-------------------------------------------------------------------------
-- We'll need these
-------------------------------------------------------------------------
import System.IO
import Graphics.Gloss



-------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------
π = pi



-------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------
window :: Display
window = InWindow "Gloss" (500, 500) (10, 10)


mainGloss :: IO ()
mainGloss = animate window white $ \ dt -> do
	rotate (90 * (sin dt)) . scale (abs $ sin dt) (abs $ cos dt) . color (makeColor (abs . sin $ dt) (abs . cos $ dt) (1.0) (1.0)) $ rectangleSolid 178 235



-------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------
main :: IO ()
main = mainGloss