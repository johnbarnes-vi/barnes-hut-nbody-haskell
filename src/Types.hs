{-# LANGUAGE DeriveGeneric #-}

module Types where

import Linear.V2 (V2)
import GHC.Generics (Generic)

-- 2D vector type using Linear's V2
type Vector2D = V2 Double

-- Particle with position, velocity, and mass
data Particle = Particle
  { position :: Vector2D
  , velocity :: Vector2D
  , mass     :: Double
  } deriving (Show, Generic)

-- Bounding box defined by center and half-width (assuming square regions)
data BoundingBox = BB
  { center    :: Vector2D
  , halfWidth :: Double
  } deriving (Show)

-- Quadtree for Barnes-Hut: Empty, Leaf with a particle, or Node with four children
data Quadtree
  = Empty BoundingBox
  | Leaf  BoundingBox Particle
  | Node  BoundingBox Quadtree Quadtree Quadtree Quadtree Double Vector2D
  -- The Double and Vector2D in Node are the total mass and center of mass
  deriving (Show)