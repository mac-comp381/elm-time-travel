--
-- The classic game of Asteroids, minus the pesky problem of dying.
-- 
-- Author: Paul Cantrell
--
module Asteroids exposing (game)

import Playground exposing (..)
import Random
import Set
import TimeTravel exposing (addTimeTravel)

game =
  { initialState = initialState
  , updateState = update
  , view = view
  }

------ PHYSICS PARAMETERS ------

asteroidCount = 10
initialAsteroidSpeed = 2
largeAsteroidRadius = 30
minAsteroidRadius = 6
asteroidComplexity = 0.4
shipSize = 16
bulletSpeed = 15
asteroidColor = (rgb 160 160 160)
shipColor = (rgb 255 120 60)
bulletColor = yellow
bulletRadius = 2

-- new const where the ship has a safe zone so asteroids can't touch it 
shipSafeRadius = 150
safeRadiusColor = rgb 255 0 0 

------ MODEL ------

type alias GameObject =
  { x : Float
  , y : Float
  , dx : Float
  , dy : Float
  , dir : Float
  , spin : Float
  , radius : Float
  , shape : List (Float, Float)
  }

type alias Model =
  { ship : GameObject
  , justFiredShot : Bool
  , asteroids : List GameObject
  , bullets : List GameObject
  }

initialState : Model
initialState =
  { ship =
      { x = 0
      , y = 0
      , dir = 0
      , spin = 0
      , dx = 0
      , dy = 0
      , radius = shipSize
      , shape = shipShape
      }
  , justFiredShot = False
  , asteroids = []
  , bullets = []
  }

shipShape =
  [ (shipSize,        0)
  , (shipSize * -0.8, shipSize * 0.7)
  , (shipSize * -0.4, 0)
  , (shipSize * -0.8, shipSize * -0.7)
  ]

bulletShape =
  [ (-bulletRadius, 0)
  , (0, bulletRadius)
  , (bulletRadius, 0)
  , (0, -bulletRadius)
  ]

------ VIEW ------
viewGameObject : Color -> Float -> GameObject -> Shape
viewGameObject color opacity obj =
  polygon color obj.shape
    |> fade opacity
    |> rotate obj.dir
    |> move obj.x obj.y
    
view computer model =
  [ rectangle black computer.screen.width computer.screen.height ]
    ++ [ viewSafeRadius model.ship ]
    ++ [model.ship      |> viewGameObject shipColor 1.0 ]
    ++ (model.asteroids |> List.map (viewGameObject asteroidColor 0.7))
    ++ (model.bullets   |> List.map (viewGameObject bulletColor 1.0))

viewSafeRadius : GameObject -> Shape
viewSafeRadius ship =
  circle safeRadiusColor shipSafeRadius
    |> fade 0.3  -- opacity control
    |> move ship.x ship.y


------ UPDATE ------

update computer model =
  model
    |> shoot computer
    |> handleMotion computer
    |> checkBulletCollisions
    |> removeAsteroidsNearShip

shoot computer model =
  if spacePressed computer then
    if model.justFiredShot then
      model  -- make user release fire button to fire again
    else
      { model
        | bullets = model.bullets ++ newBullet model
        , justFiredShot = True
      }
  else
    { model | justFiredShot = False }

newBullet model  =
  let
    shipHeadingX = cos (degrees model.ship.dir)
    shipHeadingY = sin (degrees model.ship.dir)
  in
    [
      { x = model.ship.x + shipSize * shipHeadingX
      , y = model.ship.y + shipSize * shipHeadingY
      , dx = model.ship.dx + shipHeadingX * bulletSpeed
      , dy = model.ship.dy + shipHeadingY * bulletSpeed
      , dir = 0
      , spin = 0
      , radius = bulletRadius
      , shape = bulletShape
      }
    ]

removeAsteroidsNearShip model =
  let
    isOutsideSafeRadius asteroid =
      (hypot (model.ship.x - asteroid.x) (model.ship.y - asteroid.y)) > shipSafeRadius
  in
    { model | asteroids = List.filter isOutsideSafeRadius model.asteroids }


handleMotion computer model =
  { model
    | ship      = (applyShipControls computer >> moveObject computer) model.ship
    , asteroids = List.map (moveObject computer) model.asteroids |> regenerateAsteroidsIfEmpty computer
    , bullets   = List.map (moveObject computer) model.bullets
  }

moveObject : Computer -> GameObject -> GameObject
moveObject computer obj =
  { obj
    | x = obj.x + obj.dx |> wrap (computer.screen.left - largeAsteroidRadius)   (computer.screen.right + largeAsteroidRadius)
    , y = obj.y + obj.dy |> wrap (computer.screen.bottom - largeAsteroidRadius) (computer.screen.top + largeAsteroidRadius)
    , dir = obj.dir + obj.spin
  }

applyShipControls : Computer -> GameObject -> GameObject
applyShipControls computer ship =
  let
    thrust = 0.1 * toY computer.keyboard
    dir = degrees ship.dir
  in
    { ship
      | spin = -5 * toX computer.keyboard
      , dx = ship.dx + thrust * (cos (degrees ship.dir))
      , dy = ship.dy + thrust * (sin (degrees ship.dir))
    }

checkBulletCollisions model =
  let
    (hitBullets,   freeBullets  ) = findCollided model.bullets model.asteroids
    (hitAsteroids, freeAsteroids) = findCollided model.asteroids model.bullets
  in
    { model
      | bullets = freeBullets
      , asteroids = freeAsteroids
          ++ splitAsteroids hitAsteroids
    }

findCollided shapes otherShapes =
  List.partition (anyCollide otherShapes) shapes

objectsCollide obj0 obj1 =
  (hypot (obj0.x - obj1.x) (obj0.y - obj1.y)) <= obj0.radius + obj1.radius

anyCollide otherShapes shape =
  List.any (objectsCollide shape) otherShapes

splitAsteroids bigAsteroids =
  let
    split asteroid =
      if asteroid.radius < minAsteroidRadius * 2 then
        []
      else
        [ splitAsteroid asteroid 0
        , splitAsteroid asteroid 1
        ]
  in
    List.concatMap split bigAsteroids

splitAsteroid asteroid whichHalf =
  let
    (r, θ) = toPolar (asteroid.dx, asteroid.dy)
    (newdx, newdy) = fromPolar (r, θ + 1.2 * ((toFloat whichHalf) - 0.5))
  in
    { asteroid
      | radius = asteroid.radius / (sqrt 2)
      , dx = newdx
      , dy = newdy
      , shape = eliminateSome 0.3 whichHalf asteroid.shape |> List.map (scalePoint (1 / sqrt 2))
    }

regenerateAsteroidsIfEmpty : Computer -> List GameObject -> List GameObject
regenerateAsteroidsIfEmpty computer asteroids =
  if List.isEmpty asteroids then
    generateAsteroids computer
  else
    asteroids

generateAsteroids : Computer -> List GameObject
generateAsteroids computer =
  let
    randomX = Random.float computer.screen.left computer.screen.right
    randomY = Random.float computer.screen.bottom computer.screen.top
    randomAngle = Random.float 0 (2 * pi)

    addRandomAsteroid index (list, seed0) =
      let
        (x, seed1) = Random.step randomX seed0
        (y, seed2) = Random.step randomY seed1
        (dir, seed3) = Random.step randomAngle seed2
        (spin, seed4) = Random.step randomAngle seed3
        (shape, seed5) = randomAsteroidShape largeAsteroidRadius seed4

        (dx, dy) = (initialAsteroidSpeed * cos(dir), initialAsteroidSpeed * sin(dir))
      in
        ( { x = x, y = y, dir = 0, spin = spin, dx = dx, dy = dy, radius = largeAsteroidRadius, shape = shape} :: list
        , seed5
        )

    initialSeed = Random.initialSeed 55105

    (result, lastSeed) = List.foldl addRandomAsteroid ([], initialSeed) (List.range 1 asteroidCount)
  in
    result

randomAsteroidShape : Number -> Random.Seed -> (List (Float, Float), Random.Seed)
randomAsteroidShape radius seed0 =
  let
    randomRadius = Random.float radius (radius * 1.6)
    vertexCount = 3 + round (radius * asteroidComplexity)
    addRandomVertex index (list, seed1) =
      let
        (r, seed2) = (Random.step randomRadius seed1)
        theta = (toFloat index) / (toFloat vertexCount) * pi * 2
        x = r * cos(theta)
        y = r * sin(theta)
      in
        ( (x,y) :: list
        , seed2
        )
  in
    List.foldl addRandomVertex ([], seed0) (List.range 1 vertexCount)

------ Helpers ------

wrap min max x =
  let
    diff = max - min
  in
    if max <= min then
      x
    else if x > max then
      x - diff
    else if x < min then
      x + diff
    else
      x

hypot x y =  -- mysteriously, Elm doesn't have this built in
  toPolar (x, y) |> Tuple.first

scalePoint scale (x,y) =
  (x * scale, y * scale)

eliminateSome : Float -> Int -> List a -> List a
eliminateSome fractionToRemove offset list =
  let
    ditherFilter elem (accum, error) =
      let newError = error + fractionToRemove in
        if newError >= 1 then
          (accum, newError - 1)
        else
          (elem :: accum, newError)
  in
    List.foldr ditherFilter ([], (toFloat offset) * fractionToRemove) list
      |> Tuple.first

spacePressed computer =
  Set.member " " computer.keyboard.keys
