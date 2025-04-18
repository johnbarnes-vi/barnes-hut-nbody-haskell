import Graphics.Gloss (simulate, Display(..), black, white, circleSolid, color, pictures, translate, Picture)
import Linear.V2 (V2(..))
import Linear.Vector ((*^), (^+^), (^-^), (^/), zero)
import Linear.Metric (norm, dot)
import Data.List (foldl')

-- 2D vector type using Linear's V2
type Vector2D = V2 Double

-- Particle with position, velocity, and mass
data Particle = Particle
  { position :: Vector2D
  , velocity :: Vector2D
  , mass     :: Double
  } deriving (Show)

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

-- Quadrant definition for subregions
data Quadrant = NW | NE | SW | SE deriving (Show, Eq)

-- Determine which quadrant a particle belongs to based on its position relative to the box center
whichQuadrant :: BoundingBox -> Particle -> Quadrant
whichQuadrant (BB (V2 cx cy) _) (Particle (V2 px py) _ _)
  | px < cx = if py > cy then NW else SW
  | py > cy = NE
  | otherwise = SE

-- Split a bounding box into four equal subboxes for the NW, NE, SW, SE quadrants
splitBox :: BoundingBox -> [BoundingBox]
splitBox (BB c hw) =
  let hw' = hw / 2
      offsets = [V2 (-hw') hw', V2 hw' hw', V2 (-hw') (-hw'), V2 hw' (-hw')]
  in map (\off -> BB (c + off) hw') offsets

-- Insert a particle into the quadtree, handling empty nodes, leaves, and internal nodes
insert :: Particle -> Quadtree -> Quadtree
insert p (Empty bb) = Leaf bb p
insert p (Leaf bb q) =
  let subboxes = splitBox bb
      subtrees = map Empty subboxes
      node = Node bb (head subtrees) (subtrees !! 1) (subtrees !! 2) (subtrees !! 3) 0 (V2 0 0)
      nodeWithQ = insert q node
  in insert p nodeWithQ
insert p (Node bb nw ne sw se m cm) =
  let quad = whichQuadrant bb p
      (newNw, newNe, newSw, newSe) = case quad of
        NW -> (insert p nw, ne, sw, se)
        NE -> (nw, insert p ne, sw, se)
        SW -> (nw, ne, insert p sw, se)
        SE -> (nw, ne, sw, insert p se)
      newM = m + mass p
      newCm = if newM > 0 then (m *^ cm ^+^ mass p *^ position p) ^/ newM else zero
  in Node bb newNw newNe newSw newSe newM newCm

-- Get the size (side length) of the bounding box
getSize :: BoundingBox -> Double
getSize (BB _ hw) = 2 * hw

-- Gravitational constant (set to 1 for simplicity)
g :: Double
g = 1.0

-- Accuracy parameter for Barnes-Hut approximation
theta :: Double
theta = 0.5

-- Compute direct gravitational force between two particles
directForce :: Particle -> Particle -> Vector2D
directForce p1 p2 =
  let rVec = position p2 ^-^ position p1
      r2 = dot rVec rVec
  in if r2 == 0 then zero else
       let r = sqrt r2
           forceMagnitude = g * mass p1 * mass p2 / r2
       in (forceMagnitude / r) *^ rVec

-- Compute approximate force treating a node as a single mass at its center of mass
approximateForce :: Particle -> Double -> Vector2D -> Vector2D
approximateForce p m cm =
  let rVec = cm ^-^ position p
      r2 = dot rVec rVec
  in if r2 == 0 then zero else
       let r = sqrt r2
           forceMagnitude = g * mass p * m / r2
       in (forceMagnitude / r) *^ rVec

-- Compute force on a particle using the Barnes-Hut approximation
computeForce :: Particle -> Quadtree -> Vector2D
computeForce p qt = case qt of
  Empty _ -> V2 0 0
  Leaf _ q -> if position p == position q then V2 0 0 else directForce p q
  Node bb nw ne sw se m cm ->
    let s = getSize bb
        d = norm (position p - cm)
    in if d > 0 && s / d < theta then approximateForce p m cm
       else foldl' (^+^) zero [computeForce p subtree | subtree <- [nw, ne, sw, se]]

-- Build a quadtree from a list of particles given a root bounding box
buildQuadtree :: BoundingBox -> [Particle] -> Quadtree
buildQuadtree rootBB = foldl' (flip insert) (Empty rootBB)

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

-- Convert Vector2D to (Float, Float) with scaling
toScreenCoords :: Float -> Vector2D -> (Float, Float)
toScreenCoords scale (V2 x y) = (scale * realToFrac x, scale * realToFrac y)

-- Create a Picture for a single particle
particlePicture :: Float -> Particle -> Picture
particlePicture scale p = uncurry translate (toScreenCoords scale (position p)) $ color white $ circleSolid 2

-- Render the entire scene
render :: Float -> [Particle] -> Picture
render scale particles = pictures [particlePicture scale p | p <- particles]

-- Main function
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