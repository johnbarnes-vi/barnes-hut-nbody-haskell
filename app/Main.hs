import Data.List (foldl', unfoldr)
import Graphics.Gloss (Display (..), Picture, black, circleSolid, color, pictures, simulate, translate, white)
import Linear.Metric (dot, norm)
import Linear.V2 (V2 (..), perp)
import Linear.Vector (sumV, zero, (*^), (^+^), (^-^), (^/))
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, randomR)

-- 2D vector type using Linear's V2
type Vector2D = V2 Double

-- Particle with position, velocity, mass, and radius
data Particle = Particle
  { position :: Vector2D,
    velocity :: Vector2D,
    mass :: Double,
    radius :: Double
  }
  deriving (Show)

-- Bounding box defined by center and half-width (assuming square regions)
data BoundingBox = BB
  { center :: Vector2D,
    halfWidth :: Double
  }
  deriving (Show)

-- Quadtree for Barnes-Hut: Empty, Leaf with a particle, or Node with four children
data Quadtree
  = Empty BoundingBox
  | Leaf BoundingBox Particle
  | Node BoundingBox Quadtree Quadtree Quadtree Quadtree Double Vector2D
  -- The Double and Vector2D in Node are the total mass and center of mass
  deriving (Show)

-- Quadrant definition for subregions
data Quadrant = NW | NE | SW | SE deriving (Show, Eq)

-- Determine which quadrant a particle belongs to based on its position relative to the box center
whichQuadrant :: BoundingBox -> Particle -> Quadrant
whichQuadrant (BB (V2 cx cy) _) (Particle (V2 px py) _ _ _)
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

-- Gravitational constant
g :: Double
g = 1.0

-- Accuracy parameter for Barnes-Hut approximation
theta :: Double
theta = 0.5

-- Compute direct gravitational force between two particles with softening
directForce :: Particle -> Particle -> Vector2D
directForce p1 p2 =
  let rVec = position p2 ^-^ position p1
      r2 = dot rVec rVec
      epsilon = 0.01
      softenedDenom = (r2 + epsilon * epsilon) ** 1.5
      forceMagnitude = g * mass p1 * mass p2 / softenedDenom
   in forceMagnitude *^ rVec

-- Compute approximate force treating a node as a single mass at its center of mass
approximateForce :: Particle -> Double -> Vector2D -> Vector2D
approximateForce p m cm =
  let rVec = cm ^-^ position p
      r2 = dot rVec rVec
      epsilon = 0.01
      softenedDenom = (r2 + epsilon * epsilon) ** 1.5
      forceMagnitude = g * mass p * m / softenedDenom
   in forceMagnitude *^ rVec

-- Compute force on a particle using the Barnes-Hut approximation
computeForce :: Particle -> Quadtree -> Vector2D
computeForce p qt = case qt of
  Empty _ -> V2 0 0
  Leaf _ q -> directForce p q
  Node bb nw ne sw se m cm ->
    let s = getSize bb
        d = norm (position p - cm)
     in if d > 0 && s / d < theta
          then approximateForce p m cm
          else sumV [computeForce p nw, computeForce p ne, computeForce p sw, computeForce p se]

-- Build a quadtree from a list of particles given a root bounding box
buildQuadtree :: BoundingBox -> [Particle] -> Quadtree
buildQuadtree rootBB = foldl' (flip insert) (Empty rootBB)

-- Time step size
dt :: Double
dt = 0.001

-- Update a single particle based on the net force and time step
updateParticle :: Vector2D -> Particle -> Particle
updateParticle force particle =
  let m = mass particle
      a = force ^/ m -- Acceleration: F / m
      vNew = velocity particle ^+^ (dt *^ a) -- Update velocity: v + a * dt
      pNew = position particle ^+^ (dt *^ vNew) -- Update position: p + v * dt
   in particle {velocity = vNew, position = pNew}

-- Find all pairs of particles that are colliding (distance < sum of radii)
findCollidingPairs :: [Particle] -> [(Int, Int)]
findCollidingPairs particles =
  let n = length particles
   in [ (i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1], let p1 = particles !! i
                                                               p2 = particles !! j
                                                            in norm (position p1 - position p2) < radius p1 + radius p2
      ]

-- Compute new velocities after an elastic collision based on the PDF's 7-step process
computeNewVelocities :: Particle -> Particle -> (Vector2D, Vector2D)
computeNewVelocities p1 p2 =
  let pos1 = position p1
      pos2 = position p2
      vel1 = velocity p1
      vel2 = velocity p2
      m1 = mass p1
      m2 = mass p2
      diff = pos2 - pos1
      d = norm diff
   in if d == 0
        then (vel1, vel2) -- Avoid division by zero
        else
          let -- Step 1: Unit normal and tangent vectors
              un = diff ^/ d
              ut = perp un
              -- Step 2: Velocities are already vectors
              -- Step 3: Project velocities onto normal and tangent
              v1n = dot un vel1
              v1t = dot ut vel1
              v2n = dot un vel2
              v2t = dot ut vel2
              -- Step 4: Tangential velocities unchanged
              v1t' = v1t
              v2t' = v2t
              -- Step 5: New normal velocities using 1D elastic collision formulas
              v1n' = (v1n * (m1 - m2) + 2 * m2 * v2n) / (m1 + m2)
              v2n' = (v2n * (m2 - m1) + 2 * m1 * v1n) / (m1 + m2)
              -- Step 6: Convert scalars back to vectors
              -- Step 7: Final velocity vectors
              v1' = v1n' *^ un + v1t' *^ ut
              v2' = v2n' *^ un + v2t' *^ ut
           in (v1', v2')

-- Handle a single collision by updating velocities and positions of the colliding pair
handleCollision :: [Particle] -> (Int, Int) -> [Particle]
handleCollision particles (i, j) =
  let p1 = particles !! i
      p2 = particles !! j
      pos1 = position p1
      pos2 = position p2
      diff = pos2 - pos1
      d = norm diff
      r1 = radius p1 -- 0.05
      r2 = radius p2 -- 0.05
      epsilon = 1e-10 -- Small threshold to handle near-zero distances
      (new_pos1, new_pos2) =
        if d < epsilon
          then
            let un = V2 1 0 -- Fixed direction (e.g., x-axis)
                adjustment1 = r1 *^ un
                adjustment2 = r2 *^ un
             in (pos1 - adjustment1, pos2 + adjustment2)
          else
            let overlap = (r1 + r2) - d -- Positive when overlapping
                un = diff ^/ d -- Unit vector from p1 to p2
                adjustment = (overlap / 2) *^ un
             in (pos1 - adjustment, pos2 + adjustment)
      (v1', v2') = computeNewVelocities p1 p2
   in [ if k == i
          then p {position = new_pos1, velocity = v1'}
          else
            if k == j
              then p {position = new_pos2, velocity = v2'}
              else p
        | (k, p) <- zip [0 ..] particles
      ]

-- Advance the simulation by one time step, including collision handling
step :: BoundingBox -> [Particle] -> [Particle]
step rootBB particles =
  let qt = buildQuadtree rootBB particles
      forces = map (`computeForce` qt) particles
      particles' = zipWith updateParticle forces particles -- Update based on gravity
      collidingPairs = findCollidingPairs particles' -- Detect collisions
      particles'' = foldl handleCollision particles' collidingPairs -- Handle collisions
   in particles''

-- Convert Vector2D to (Float, Float) with scaling
toScreenCoords :: Float -> Vector2D -> (Float, Float)
toScreenCoords scale (V2 x y) = (scale * realToFrac x, scale * realToFrac y)

-- Create a Picture for a single particle
particlePicture :: Float -> Particle -> Picture
particlePicture scale p = uncurry translate (toScreenCoords scale (position p)) $ color white $ circleSolid 2

-- Render the entire scene
render :: Float -> [Particle] -> Picture
render scale particles = pictures [particlePicture scale p | p <- particles]

-- Generate a random particle with properties within specified ranges
randomParticle :: StdGen -> (Particle, StdGen)
randomParticle gen =
  let (x, gen1) = randomR (-10, 10) gen -- Position x: [-10, 10]
      (y, gen2) = randomR (-10, 10) gen1 -- Position y: [-10, 10]
      (vx, gen3) = randomR (0, 0) gen2 -- Velocity x: [-1, 1]
      (vy, gen4) = randomR (0, 0) gen3 -- Velocity y: [-1, 1]
      (m, gen5) = randomR (1, 1) gen4 -- Mass: [1, 10]
      r = 0.05 -- Fixed radius
   in (Particle {position = V2 x y, velocity = V2 vx vy, mass = m, radius = r}, gen5)

-- Generate a list of n random particles
generateParticles :: Int -> StdGen -> [Particle]
generateParticles n gen = take n $ unfoldr (Just . randomParticle) gen

-- Main function to run the simulation
main :: IO ()
main = do
  args <- getArgs
  gen <- getStdGen
  let n = read (head args) :: Int
      particles = generateParticles n gen
      display = InWindow "Barnes-Hut Simulation with Collisions" (800, 800) (10, 10)
      bgColor = black
      fps = 60
      renderFunc = render scale
      updateFunc _ _ = step boundingBox
      scale = 40.0
      boundingBox = BB {center = V2 0.0 0.0, halfWidth = 10.0}
   in simulate display bgColor fps particles renderFunc updateFunc