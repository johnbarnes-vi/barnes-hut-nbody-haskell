module Main where

import Linear.V2 ( V2(..) )
import Graphics.Gloss ( circle, Picture )

main :: IO ()
main = do
  let v = V2 1.0 2.0 :: V2 Double
  print v
  let pic = circle 50 :: Picture
  print pic