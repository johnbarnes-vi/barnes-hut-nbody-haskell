Barnes-Hut N-Body Simulator

This project implements a 2D Barnes-Hut n-body simulation in Haskell, visualizing gravitational interactions between particles using the `gloss` library. The simulator leverages a quadtree data structure to efficiently approximate gravitational forces, reducing computational complexity from O(n²) to O(n log n).

## Table of Contents

- Overview
- Features
- Installation
- Usage
- Code Structure
- Contributing
- License

## Overview

The Barnes-Hut algorithm is a method for simulating the gravitational forces in an n-body system. By grouping distant particles into a single node in a quadtree, the algorithm approximates their collective gravitational effect, significantly improving performance for large numbers of particles.

This implementation provides a simple, readable, and concise demonstration of the algorithm in Haskell, contained within a single `Main.hs` file for ease of understanding and modification.

## Features

- **Quadtree Construction**: Efficiently partitions space to group particles.
- **Force Approximation**: Uses the Barnes-Hut approximation to compute gravitational forces.
- **Simulation Step**: Updates particle positions and velocities based on computed forces.
- **Rendering**: Visualizes particles using `gloss`, with positions scaled for display.
- **Configurable Parameters**: Easily adjust simulation parameters like time step, gravitational constant, and accuracy threshold.

## Installation

To run this simulation, you need to have Haskell installed along with the required libraries. Follow these steps:

1. **Install Haskell**: Ensure you have GHC (Glasgow Haskell Compiler) installed. You can download it from Haskell.org.

2. **Install Dependencies**: This project relies on the `gloss` and `linear` libraries. Install them using Cabal:

   ```bash
   cabal update
   cabal install gloss linear
   ```

3. **Clone the Repository**:

   ```bash
   git clone https://github.com/yourusername/barnes-hut-simulator.git
   cd barnes-hut-simulator
   ```

## Usage

To run the simulation, execute the following command in the project directory:

```bash
ghc -o simulator Main.hs
./simulator
```

This will launch a window displaying the simulation of two particles interacting gravitationally. You can modify the `initialParticles` list in `Main.hs` to simulate more bodies.

### Configuration

- **Display Settings**: Window size and position can be adjusted in the `display` variable.
- **Simulation Parameters**:
  - `fps`: Frames per second for the simulation.
  - `dt`: Time step size for each simulation update.
  - `g`: Gravitational constant.
  - `theta`: Accuracy parameter for the Barnes-Hut approximation.
  - `scale`: Scaling factor for rendering particle positions.

## Code Structure

The entire simulation is contained within `Main.hs`, organized as follows:

- **Type Definitions**:

  - `Vector2D`: 2D vectors using `Linear.V2`.
  - `Particle`: Represents a particle with position, velocity, and mass.
  - `BoundingBox`: Defines a square region in space.
  - `Quadtree`: Data structure for spatial partitioning.
  - `Quadrant`: Enumerates the four quadrants of a bounding box.

- **Quadtree Operations**:

  - `whichQuadrant`: Determines the quadrant a particle belongs to.
  - `splitBox`: Divides a bounding box into four sub-boxes.
  - `insert`: Inserts a particle into the quadtree.
  - `buildQuadtree`: Constructs a quadtree from a list of particles.

- **Force Calculations**:

  - `directForce`: Computes the exact gravitational force between two particles.
  - `approximateForce`: Approximates the force from a group of particles.
  - `computeForce`: Uses the Barnes-Hut approximation to compute the force on a particle.

- **Simulation Logic**:

  - `updateParticle`: Updates a particle's velocity and position based on the net force.
  - `step`: Advances the simulation by one time step.

- **Rendering**:

  - `toScreenCoords`: Scales particle positions for display.
  - `particlePicture`: Creates a `Picture` for a single particle.
  - `render`: Renders all particles.

- **Main Function**: Sets up and runs the simulation using `gloss`.