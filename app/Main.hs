module Main where

import Graphics.Gloss (simulate, Display(..), black)
import Types (Particle(..), BoundingBox(..))
import Simulation (step)
import Render (render)
import Linear.V2 (V2(..))

main :: IO ()
main = simulate display bgColor fps initialState renderFunc updateFunc
  where
    display = InWindow "Barnes-Hut Simulation" (800, 800) (10, 10)
    bgColor = black
    fps = 60
    initialState = initialParticles
    renderFunc = render scale
    updateFunc _ _ = step boundingBox
    scale = 40.0
    boundingBox = BB { center = V2 0.0 0.0, halfWidth = 10.0 }
    initialParticles = [particle1, particle2]
    particle1 = Particle { position = V2 1.0 0.0, velocity = V2 0.0 0.5, mass = 1.0 }
    particle2 = Particle { position = V2 (-1.0) 0.0, velocity = V2 0.0 (-0.5), mass = 5.0 }