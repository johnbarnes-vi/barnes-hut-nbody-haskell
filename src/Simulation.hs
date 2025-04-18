module Simulation where

import Types (Particle(..), Vector2D, BoundingBox)
import Quadtree (buildQuadtree, computeForce)
import Linear.Vector ((*^), (^+^), (^/))

-- Time step size (configurable constant for now)
dt :: Double
dt = 0.01

-- Update a single particle based on the net force and time step
updateParticle :: Vector2D -> Particle -> Particle
updateParticle force particle =
  let m = mass particle
      a = force ^/ m          -- Acceleration: F / m
      vNew = velocity particle ^+^ (dt *^ a)  -- Update velocity: v + a * dt
      pNew = position particle ^+^ (dt *^ vNew)  -- Update position: p + v * dt
  in particle { velocity = vNew, position = pNew }

-- Advance the simulation by one time step
step :: BoundingBox -> [Particle] -> [Particle]
step rootBB particles =
  let qt = buildQuadtree rootBB particles  -- Construct quadtree from current state
      forces = map (`computeForce` qt) particles  -- Compute forces for all particles
  in zipWith updateParticle forces particles  -- Update all particles with their forces