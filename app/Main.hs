module Main where

import Linear.V2 ( V2(..) )
import Graphics.Gloss ( display, Display(InWindow), white, circle, pictures, translate, color, red, blue, Color, Picture )

coloredCircle :: Color -> V2 Float -> Float -> Picture
coloredCircle col pos radius = let V2 x y = pos in translate x y (color col (circle radius))

main :: IO ()
main = do
  let v1 = V2 10.0 20.0 :: V2 Float
  let v2 = V2 30.0 40.0 :: V2 Float
  let v3 = v1 + v2
  putStrLn $ "Circle 1 at " ++ show v1
  putStrLn $ "Circle 2 at " ++ show v2
  putStrLn $ "Sum of vectors: " ++ show v3
  let pic = pictures [ coloredCircle red v1 50, coloredCircle blue v2 500 ]
  display (InWindow "Vector Circles" (400, 400) (10, 10)) white pic