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
import Graphics.Gloss.Data.Picture (line)
import Graphics.Gloss.Geometry.Angle (degToRad, radToDeg, normaliseAngle)
import Graphics.Gloss.Interface.IO.Game



-------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------
π = pi
e = exp 1

size = (floor $ 720*1.5, floor $ 480*1.5)


-- Applies a function iteratively n times
ntimes :: Int -> (a -> a) -> a -> a
ntimes n f = foldr (.) id $ replicate n f


-- TODO | Transition between colours
palette = [black, red, green, rose, orange, chartreuse, aquamarine, azure, violet] :: [Color]


--
timer :: Float -> Float -> Float -> Color -> Color -> Picture
timer r start gap bgCol wedgeCol = pictures [ color bgCol $ circleSolid (2*r), --(color bgCol $ thickArc (start + gap) start (2*r) r),
											 (color wedgeCol $ thickArc (clamp start) (clamp $ start + gap) r (2*r)) ]
	where
		clamp = radToDeg . normaliseAngle . degToRad -- Clamp angle to [0, 360)


--
-- TODO | Use scan for intermediate values (?)
-- TODO | Decide between percentages and angles
-- TODO | Verify percentages (1.0, 100.0 or 360.0)
-- TODO | Start angle
piechart :: [(Float, Color)] -> Float -> Picture
piechart items r = pictures . snd $ foldl (\ (sum, pict) (θ, clr) -> (sum+θ, wedge r sum (sum+θ) clr : pict) ) (0, []) items
--piechart items r = pictures $ map (\ (θ, clr) -> wedge r θ) angles
	where
		wedge rad fr to col = color col . thickArc fr to rad $ 2*rad
		angles = scanl (+) 0 $ map snd items


--clock 


--
slices :: Float -> [Float]
slices θ = [10-θ, 20+θ, 15, 25-θ, 60+θ, 75, 132, 15, 8]



-------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------
window :: Display
window = InWindow "Gloss" size (10, 10)


--
-- The mysterious 'world' type appears to be a simple type variable
-- TODO | Create custom type or newtype for world (?)
simulate :: IO ()
simulate = playIO
	display 	--
	white 		-- Background colour
	60			-- FPS (simulation steps per second, technically)
	world 		-- Initial world
	render		-- Converts world to Picture
	handleEvent --
	advance 	-- Advances the world to the next simulation step
	where
		display  = InWindow "Simulator" size (25, 25)
		world 	 = 0
		render w = return 0
		handleEvent e w = return $
		advance t w = return w



--
-- TODO | Dealing with previous state
mainGloss :: IO ()
mainGloss = animate window white $ \dt -> pictures [
	-- Rectangles
	translate (-300) 0 . tweak dt . color (makeColor (abs . sin $ dt) (abs . cos $ dt) (1.0) (1.0)) $ rectangleSolid 178 235,
	translate (-300) 0 . tweak dt . color white $ rectangleSolid (178 - 80) (235 - 80),
	-- Sine graph
	line [(25*x, 25 * sin x) | x <- [5,5.5..20]],
	-- Arc
	let angle = 1/4 * 360 * dt in translate 50 (-150) . color (palette !! ((floor angle `div` 360) `mod` length palette)) $ thickArc 0 angle 150 15.2,
	-- Timer
	translate 300 120 $ timer 40 0 (360 - 1/4 * 360 * dt) red chartreuse,
	-- Text
	translate (-240) (220-dt*10) . scale 0.2 0.2 . color blue $ text (show dt ++ "s") , -- "Gloss Sample 1.0",
	-- Pie chart
	let θ = 8*(sin $ 1/80 * 360 * dt) in piechart (zip (slices θ) $ map (mixColors 3 7 (makeColor (cos $ 1/80 * 360 * dt) (sin $ 1/40 * 360 * dt) (0.75) 1.0)) palette) 85 ]
		where
			tweak dt = rotate (45 * (2 * sin dt)) . scale (abs $ sin dt) (abs $ cos dt)



-------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------
main :: IO ()
main = mainGloss