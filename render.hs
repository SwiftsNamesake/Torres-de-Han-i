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
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
--import Graphics.Gloss



-------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------
redraw :: DisplayCallback
redraw = do return ()




-------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------
main :: IO ()
main = do
	createWindow "Torres de Hanoi"
	displayCallback $= redraw
	mainLoop