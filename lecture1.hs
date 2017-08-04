{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

bottomCircle :: Color -> Picture
bottomCircle c = colored c (translated 0 (-1.5) (solidCircle 1))

topCircle :: Color -> Picture
topCircle c = colored c (translated 0 1.5 (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 5.5

trafficLight :: Bool -> Picture
trafficLight True = bottomCircle green & topCircle black & frame
trafficLight False = bottomCircle black & topCircle red & frame

lights :: Integer -> Picture
lights 0 = blank
-- lights n = trafficLight True & translated 3 0 (lights (n-1))
lights n = translated 3 0 (lights (n-1)) & trafficLight True

spread :: Picture -> Double -> Integer -> Picture
spread _ _ 0    = blank
spread  pic dx n = pic & translated dx 0 (spread pic dx (n - 1))

tree :: Integer -> Picture
tree 0 = blank
tree n = path [(0, 0), (0, 1)] & translated 0 1 (
  rotated (pi/10) (tree (n - 1)) & rotated (-pi/10) (tree (n - 1)))

{-
ourPicture :: Picture
ourPicture = trafficLight
  where
    bottomCircleGreen = colored green (translated 0 (-1.5) $ solidCircle 1)
    topCircleRed      = colored red (translated 0 1.5 $ solidCircle 1)
    frame             = rectangle 2.5 5.5
    trafficLight      = bottomCircleGreen & topCircleRed & frame
-}

ourPicture :: Picture
-- this will run infinitely
-- ourPicture = lights (-1)
-- ourPicture = lights 3
ourPicture = spread (trafficLight True) 3 4


trafficController :: Double -> Picture
trafficController t
  | round (t / 3) `mod` 2 == 0 = trafficLight True
  | otherwise                  = trafficLight False

main :: IO ()
-- main = animationOf trafficController
-- main = drawingOf ourPicture
-- main = drawingOf $ path [(0, 0), (0, 1)]
main = drawingOf (tree 10)
