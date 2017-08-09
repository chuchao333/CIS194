{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise3


-- Exercise 1

lightBulb ::Color -> Double -> Picture
lightBulb color dy = colored color (translated 0 dy (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

trafficLight :: Color -> Color -> Color -> Picture
trafficLight botColor midColor topColor =
  lightBulb botColor (-2.5) &
  lightBulb midColor 0 &
  lightBulb topColor 2.5 &
  frame

trafficController :: Integer -> Picture
trafficController s
  | s >= 0 && s <= 2 = trafficLight green black black
  | s == 3           = trafficLight black orange black
  | s >= 4 && s <= 6 = trafficLight black black red
  | otherwise        = trafficLight black orange red

trafficLightAnimation :: Double -> Picture
trafficLightAnimation t = trafficController (round t `mod` 8)

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

tree :: Picture -> Integer -> Picture
tree bloom 0 = bloom
tree bloom n = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree bloom (n-1)) & rotated (- pi/10) (tree bloom (n-1)))
  
blossom :: Double -> Picture
blossom t = tree bloom 8
  where
    bloom = colored yellow (solidCircle ((min t 10) / 50))
  
exercise2 :: IO ()
exercise2 = animationOf blossom

-- Exercise 3

unitRectangle :: Picture
unitRectangle = solidRectangle 1 1

wall, ground, storage, box :: Picture
wall    = colored (grey 0.4) unitRectangle
ground  = colored yellow unitRectangle
storage = solidCircle 0.4 & ground
box     = colored brown unitRectangle

drawTile :: Integer -> Picture
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box
drawTile _ = blank

drawTileAt :: Integer -> Integer -> Picture
drawTileAt row col =
  translated (fromIntegral row) (fromIntegral col) (drawTile (maze row col))

drawCols :: Integer -> Integer -> Picture
drawCols _ 11 = blank
drawCols row col = drawTileAt row col & drawCols row (col + 1)

drawRows :: Integer -> Picture
drawRows 11  = blank
drawRows row = drawCols row (-10) & drawRows (row + 1)

maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
         
pictureOfMaze :: Picture
pictureOfMaze = drawRows (-10)

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze
 
