--
-- Tower of Hanoi - render.hs
-- Graphics utilities
--
-- Jonatan H Sundqvist
-- October 29 2014
--

-- TODO | - Menu
--        - Parse arguments, choose main function

-- SPEC | -
--        -



module Render where



-------------------------------------------------------------------------
-- We'll need these
-------------------------------------------------------------------------
import Reason
import System.IO
import Graphics.Gloss
import Graphics.Gloss.Data.Picture (line)
import Graphics.Gloss.Geometry.Angle (degToRad, radToDeg, normaliseAngle)
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as Map -- (lookup, fromList)



-------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------
π = pi 		-- Another slice, please!
e = exp 1 	-- 

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


--
-- TODO | Consistent terminology (w.r.t. peg)
drawBoard :: Board -> Picture
drawBoard (Board a b c) 	= pictures . concat . moveRight . map renderPeg $ [a, b, c]
	where
		renderPeg 	= addPole . map renderDisk . zip [0..] . reverse
		renderDisk  = \ (n, disk) -> translate 0 (height * fromIntegral n) . color (colour n) . rectangleSolid (20 * fromIntegral disk) $ height
		moveRight 	= map (\ (n, disks) -> map (translate (deltaPeg * fromIntegral n) 0) disks) . zip [0..]
		addPole		= ((translate 0 (poleHeight/2 - height/2) $ rectangleSolid 10 poleHeight) :)
		poleHeight 	= height * largest + 25 						-- 
		colour n 	= if n `mod` 2 == 0 then chartreuse else rose  	-- 
		height 		= 15 											-- Height of each disk
		largest 	= fromIntegral . maximum $ 0 : concat [a, b, c] -- Largest disk on the Board (0 if empty)
		deltaPeg 	= 120											-- Distance between adjacent pegs


-------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------
window :: Display
window = InWindow "Gloss" size (10, 10)


--
-- TODO | Use records or normal constructor (both?)
-- TODO | Key map
data World = World
	Float		-- Time
	String		-- Text
	Path		-- Path
	(Int, Int)	-- Size
	(Int, Int)	-- Position


--
--twinkle


--
mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a, b) = (f a, f b)


--
-- The mysterious 'world' type appears to be a simple type variable
-- TODO | Create custom type or newtype for world (?)
-- TODO | Separate event handlers (less messy)
-- TODO | Update size
simulate :: Picture -> IO ()
simulate image = playIO
	display 	--
	white 		-- Background colour
	60			-- FPS (simulation steps per second, technically)
	world 		-- Initial world
	render		-- Converts world to Picture
	handleEvent --
	advance 	-- Advances the world to the next simulation step
	where
		display  = InWindow "Simulator" size (25, 25)
		world 	 = World 0 "X: ? | Y: ?" [] (0, 0) size
		render wd = let World t s path (x', y') sz = wd in return . pictures $ [
			translate (-45) (-45)  . rotate (t * 1/4 * 360) . rectangleSolid 45 $ 45,
			translate (-120) (150) . scale 0.15 0.15 . text $ "X: " ++ show x' ++ " | Y: " ++ show y', -- TODO | printf (?)
			translate (fromIntegral x') (fromIntegral y') . circleSolid $ 12,
			color red . line $ path,
			color blue . line . map (mapPair (+12)) $ path] ++ let (w, h) = mapPair fromIntegral sz; (x, y) = mapPair fromIntegral (x', y') in [
				color red . line $ [(x, h/2), (x, -h/2)],
				color blue . line $ [(w/2, y), (-w/2, y)],
			let
				θ 			= 8 * (sin $ 1/95 * 360 * t)
				additive 	= makeColor (cos $ 1/160 * 360 * t) (sin $ 1/80 * 360 * t) (0.75) 1.0
				radiate 	= mixColors 3 7 additive
				(w, h) 		= mapPair fromIntegral sz
				radius 		= 40 + 220 * (abs $ fromIntegral x') / w
				transform 	= rotate $ 360 * (fromIntegral y') / h
			in transform $ piechart (zip (slices θ) $ map radiate palette) radius ]
		handleEvent ev wd@(World t s path p@(x', y') sz) = case ev of
			EventMotion (x, y) -> return $ World t s path (floor x, floor y) sz
			EventResize (w, h) -> return $ World t s path p (w, h)
			EventKey key state mod (a, b) -> return $ case key of
				MouseButton LeftButton 	-> World t s (mapPair fromIntegral p : path) p sz
				_						-> wd
		advance t (World t' s path p sz) = return $ World (t'+t) s path p sz

		--image = loadBMP "C:/Users/Jonatan/Pictures/homer.bmp"
		-- bitmapOfBytestring


--
-- TODO | Dealing with previous state
mainGloss :: IO ()
mainGloss = animate window white $ \dt -> pictures [
	translate (-300) 0 . tweak dt . color (makeColor (abs . sin $ dt) (abs . cos $ dt) (1.0) (1.0)) $ rectangleSolid 178 235, -- Rectangles
	translate (-300) 0 . tweak dt . color white $ rectangleSolid (178 - 80) (235 - 80),
	line [(25*x, 25 * sin x) | x <- [5,5.5..20]], -- Sine graph
	let angle = 1/4 * 360 * dt in translate 50 (-150) . color (palette !! ((floor angle `div` 360) `mod` length palette)) $ thickArc 0 angle 150 15.2, -- Arc
	translate 300 120 $ timer 40 0 (360 - 1/4 * 360 * dt) red chartreuse, -- Timer
	translate (-240) (220-dt*10) . scale 0.2 0.2 . color blue $ text (show dt ++ "s") , -- Text
	let θ = 8*(sin $ 1/80 * 360 * dt) in piechart (zip (slices θ) $ map (mixColors 3 7 (makeColor (cos $ 1/80 * 360 * dt) (sin $ 1/40 * 360 * dt) (0.75) 1.0)) palette) 85 ]
		where
			tweak dt = rotate (45 * (2 * sin dt)) . scale (abs $ sin dt) (abs $ cos dt)



type KeyMap = Map.Map Key Bool
data Game = Game
	Board
	String 	-- Name
	Int 	-- Score
	KeyMap	--


--
choose :: a -> a -> Bool -> a
choose a b True	 = a
choose a b False = b

--
mainHanoi :: IO ()
mainHanoi = playIO
	display 	--
	white 		-- Background colour
	60			-- FPS (simulation steps per second, technically)
	world 		-- Initial world
	render		-- Converts world to Picture
	handleEvent -- 
	advance 	-- Advances the world to the next simulation step
	where
		display  = InWindow "Simulator" size (25, 25)
		board = createGame 3
		render (Game bd _ _ km) = do
			let board = drawBoard bd
			let keys = map (\ key -> let SpecialKey which = key in (which, Map.findWithDefault False key km)) [SpecialKey KeyUp, SpecialKey KeyDown]
			let colour = choose black red :: Bool -> Color
			let rendered = map (\ (n, (key, state)) -> translate (-465) (250-fromIntegral n * 35) . (color $ colour state) . scale 0.2 0.2 . text . show $ key) $ zip [1..] keys -- TODO | Abstract mapping with index
			return . pictures $ board : rendered
		world = Game board "Jonatan" 0 $ Map.fromList []
		handleEvent ev (Game bd nm sc km) = updateKeymap ev (Game bd nm sc km) >>= \ (Game (Board a b c) nm sc km) -> return $ case ev of
			EventKey (SpecialKey KeyUp) Down _ _ -> Game (Board (drop 1 a) b c) nm sc km
			EventKey (SpecialKey KeyDown) Down _ _ -> Game (Board b a c) nm sc km
			_ -> Game (Board a b c) nm sc km
		updateKeymap ev (Game bd nm sc km) = return $ case ev of
			EventKey key state _ _ -> Game bd nm sc $ Map.insert key (state == Down) km
			_ 					   -> Game bd nm sc km
		advance _ = return



-------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------
main :: IO ()
main = do
	mainHanoi
	--image <- loadBMP "C:/Users/Jonatan/Pictures/homer.bmp"
	--Render.simulate image
--main = mainGloss