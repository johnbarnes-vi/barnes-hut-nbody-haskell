module Render (render) where

import Graphics.Gloss ( white, circleSolid, color, pictures, translate, Picture )
import Types (Particle(..), Vector2D)
import Linear.V2 (V2(..))

-- Convert Vector2D to (Float, Float) with scaling
toScreenCoords :: Float -> Vector2D -> (Float, Float)
toScreenCoords scale (V2 x y) = (scale * realToFrac x, scale * realToFrac y)

-- Create a Picture for a single particle
particlePicture :: Float -> Particle -> Picture
particlePicture scale p = uncurry translate (toScreenCoords scale (position p)) $ color white $ circleSolid 2

-- Render the entire scene
render :: Float -> [Particle] -> Picture
render scale particles = pictures [particlePicture scale p | p <- particles]