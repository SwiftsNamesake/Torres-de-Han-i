--
-- Tower of Hanoi - recreation.hs
-- Experiments and frivolities ahead
--
-- Jonatan H Sundqvist
-- November 2 2014
--

-- TODO | -
--        -

-- SPEC | -
--        -



module Recreation where



-------------------------------------------------------------------------
-- We'll need these
-------------------------------------------------------------------------
import System.IO
import Data.List (groupBy)
import Data.Char (isSpace, isAlpha)
import Graphics.Gloss
import Graphics.Gloss.Data.Picture (line)
import Graphics.Gloss.Geometry.Angle (degToRad, radToDeg, normaliseAngle)



-------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------


--
type Map = Path


--
-- TODO | cf. mapFst and mapSnd
cutAt :: Eq a => a -> [a] -> ([a], [a])
cutAt x list = let (a, b) = break (== x) list in (a, tail b)
--cutAt x list = (takeWhile (/= x) list, tail $ dropWhile (/= x) list)


--
-- TODO | Extract 'split' routine (groupBy (/= ';'))
-- TODO | Error handling
-- TODO | Extract pure code (string parsing)
loadMap :: String -> IO Map
loadMap fn = do
	file <- readFile fn
	let tokens = map (filter (/= ';')) . groupBy (\ a b -> b /= ';') . filter (not . isSpace) $ file
	print tokens
	let points = map (\ str -> let (x, y) = cutAt ',' str in (read x, -read y)) $ tokens
	return points


-------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------
main :: IO ()
main = do
	markers <- loadMap $ ["sweden.txt", "generated.txt", "norway.txt"] !! 2
	print markers
	animate window white $ render markers
	where
		window 				= [ InWindow "Cartographer" size (25, 25), FullScreen (450, 450)] !! 0
		render markers dt 	= pictures $ [dotGrid 20 20 32]  --[outline markers, silhouette markers, dots markers 0 0, dots markers dx 0]
		outline markers 	= scale 0.5 0.5 . color black $ lineLoop markers
		silhouette markers	= scale 0.5 0.5 . color (makeColor 0.35 0.45 0.82 0.7) . translate dx 0 $ polygon markers
		dots markers dx dy	= scale 0.5 0.5 . pictures $ map (\(x,y) -> color (makeColor (sin x) (cos y) 0.86 1.0) . translate (dx+x) (dy+y) $ circleSolid 5) markers
		dx 					= 300
		size 				= (700, 700)
		dotGrid rws cls pad = pictures [ color (makeColor (sin (r*pad)) (cos (c*pad)) 0.86 1.0) . translate (r*pad) (c*pad) $ circleSolid 5 | r <- [1..rws], c <- [1..cls] ]