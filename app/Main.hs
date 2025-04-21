import Data.List (foldl', mapAccumL)
import Graphics.Gloss (Display (..), Picture, black, circleSolid, color, pictures, simulate, translate, white)
import Linear.Metric (dot, norm)
import Linear.V2 (V2 (..), perp)
import Linear.Vector (sumV, zero, (*^), (^/))
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, randomR)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST ( runST )

-- 2D vector type using Linear's V2
type Vector2D = V2 Double

-- Particle with position, velocity, mass, and radius
data Particle = Particle
  { 
    index :: Int,
    position :: Vector2D,
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
whichQuadrant (BB (V2 cx cy) _) (Particle _ (V2 px py) _ _ _)
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
      newCm = if newM > 0 then (m *^ cm + mass p *^ position p) ^/ newM else zero
   in Node bb newNw newNe newSw newSe newM newCm

-- Helper function to compute softened gravitational force
forceOn :: Particle -> Double -> Vector2D -> Vector2D
forceOn particle otherMass otherPosition =
  let gravitationalConstant = 1.0
      rVec = otherPosition - position particle
      r2 = dot rVec rVec
      epsilon = 0.01
      softenedDenom = (r2 + epsilon * epsilon) ** 1.5
      forceMagnitude = (gravitationalConstant * mass particle * otherMass) / softenedDenom
   in forceMagnitude *^ rVec

-- Compute force on a particle using the Barnes-Hut approximation
traverseForceOn :: Particle -> Quadtree -> Vector2D
traverseForceOn particle quadTree = case quadTree of
  Empty _ -> V2 0 0
  Leaf _ otherParticle ->
    forceOn particle (mass otherParticle) (position otherParticle)
  Node (BB _ halfWidth) nw ne sw se aggregateMass aggregatePosition ->
    let l = 2 * halfWidth
        d = norm (position particle - aggregatePosition)
        theta = 0.5
     in if d > 0 && l / d < theta
          then forceOn particle aggregateMass aggregatePosition
          else sumV [traverseForceOn particle nw, traverseForceOn particle ne, traverseForceOn particle sw, traverseForceOn particle se]

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
      vNew = velocity particle + (dt *^ a) -- Update velocity: v + a * dt
      pNew = position particle + (dt *^ vNew) -- Update position: p + v * dt
   in particle {velocity = vNew, position = pNew}

-- Find all pairs of particles that are colliding (distance < sum of radii)
findCollidingPairs :: [Particle] -> [(Int, Int)]
findCollidingPairs particles =
  let cellSize = 0.1
      toCell :: Vector2D -> (Int, Int)
      toCell (V2 x y) = (floor (x / cellSize), floor (y / cellSize))
      -- Build grid: Map (Int, Int) to [Particle]
      grid = foldl' (\m p -> Map.insertWith (++) (toCell (position p)) [p] m)
                    Map.empty particles
      -- Get all nine neighboring cells (including current cell)
      getNeighbors (ix, iy) = [(ix + dx, iy + dy) | dx <- [-1..1], dy <- [-1..1]]
      -- For each particle, find colliding pairs with higher indices
      pairs = concatMap (\p ->
                let i = index p
                    cell = toCell (position p)
                    neighborCells = getNeighbors cell
                    neighbors = concat [Map.findWithDefault [] c grid | c <- neighborCells]
                 in [(i, j) | q <- neighbors,
                              let j = index q,
                              j > i,
                              norm (position p - position q) < 0.1]
               ) particles
   in pairs

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

-- Helper function to compute new positions for colliding particles
computeNewPositions :: Particle -> Particle -> (Vector2D, Vector2D)
computeNewPositions p1 p2 =
  let pos1 = position p1
      pos2 = position p2
      diff = pos2 - pos1
      d = norm diff
      r1 = radius p1
      r2 = radius p2
      epsilon = 1e-10
  in if d < epsilon
       then
         let un = V2 1 0
             adjustment1 = r1 *^ un
             adjustment2 = r2 *^ un
         in (pos1 - adjustment1, pos2 + adjustment2)
       else
         let overlap = (r1 + r2) - d
             un = diff ^/ d
             adjustment = (overlap / 2) *^ un
         in (pos1 - adjustment, pos2 + adjustment)

-- Modified step function with efficient collision handling
step :: BoundingBox -> [Particle] -> [Particle]
step rootBB particles = runST $ do
  let quadTree = buildQuadtree rootBB particles
      forces = map (`traverseForceOn` quadTree) particles
      particles' = zipWith updateParticle forces particles
      collidingPairs = findCollidingPairs particles'
  
  -- Convert list to mutable vector
  v <- V.thaw (V.fromList particles')  -- O(N)
  
  -- Handle collisions on the mutable vector
  mapM_ (\(i, j) -> do
    p1 <- MV.read v i
    p2 <- MV.read v j
    let (pos1', pos2') = computeNewPositions p1 p2
        (v1', v2') = computeNewVelocities p1 p2
    MV.write v i (p1 {position = pos1', velocity = v1'})
    MV.write v j (p2 {position = pos2', velocity = v2'})
    ) collidingPairs  -- O(K)
  
  -- Convert mutable vector back to list
  V.toList <$> V.freeze v  -- O(N)

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
randomParticle :: Int -> StdGen -> (Particle, StdGen)
randomParticle idx gen =
  let (x, gen1) = randomR (-10, 10) gen
      (y, gen2) = randomR (-10, 10) gen1
      (vx, gen3) = randomR (0, 0) gen2
      (vy, gen4) = randomR (0, 0) gen3
      (m, gen5) = randomR (1, 1) gen4
      r = 0.05
   in (Particle {index = idx, position = V2 x y, velocity = V2 vx vy, mass = m, radius = r}, gen5)

-- Generate a list of n random particles
generateParticles :: Int -> StdGen -> [Particle]
generateParticles n gen =
  snd $ mapAccumL (\g i -> let (p, g') = randomParticle i g in (g', p)) gen [0..n-1]

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