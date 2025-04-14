--
-- The classic game of Asteroids, minus the pesky problem of dying.
-- 
-- Author: Evan Czaplicki, with modifications by Paul Cantrell
--         Based on https://elm-lang.org/examples/mario
--
module Mario exposing (game)
import Playground exposing (..)

-- PHYSICS PARAMETERS

runSpeed = 3
coast = 0.9
jumpPower = 8
jumpCutoff = 0.5
gravity = 0.2


-- MAIN

game =
  { initialState = initialState
  , updateState = update
  , view = view
  }

initialState =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  , trace = []
  }

type XDirection = Left | Right


lavas =
  [ { x = -700, width = 150}
  , { x = -100, width = 120}
  , { x = 700, width = 180}
  ]
portals = [
  { x = 300, width = 150 },
  { x = -300, width = 150 }
  ]

-- Functions to check if mario is in the lava or the portal 

isInLava : Float -> Float -> Bool
isInLava x y =
    List.any (\lava ->
        x >= (lava.x - lava.width / 2)&& x <= (lava.x + lava.width/2) && y == 0
    ) lavas

isInPortal :Float -> Float -> Bool
isInPortal x y =
    List.any (\portal ->
        x >= (portal.x - portal.width / 2)  && x <= (portal.x + portal.width/2) && y == 0
    )portals


-- VIEW

view computer mario =
  let
    w = computer.screen.width
    h = computer.screen.height
    b = computer.screen.bottom
    convertY y = (b + 76 + y)
  
    shapes = 
      [ rectangle (rgb 174 238 238) w h  -- sky
      , rectangle (rgb 74 163 41) w 100  -- ground
          |> moveY b
      , mario.trace
          |> pathToPolygonVertices 1.5
          |> polygon black
          |> move 0 (b + 76)
          |> fade 0.5
      , marioSpriteName mario
          |> image 70 70
          |> move mario.x (b + 76 + mario.y)
      
      ]
      ++
      -- draw lavas
      List.map (\lava ->
          rectangle (rgb 255 70 0) lava.width 100
              |> move lava.x b  
      ) lavas
      ++
      -- draw portal
      List.map (\portal ->
          rectangle (rgb 0 0 0) portal.width 100
              |> move portal.x b  
      ) portals 
  in 
    shapes


marioSpriteName mario =
  let
    stance =
      if mario.y > 0 then
        "jump"
      else if mario.vx /= 0 then
        "walk"
      else
        "stand"
    direction =
      case mario.dir of
        Left -> "left"
        Right -> "right"
  in
    "https://elm-lang.org/images/mario/" ++ stance ++ "/" ++ direction ++ ".gif"


-- UPDATE

update computer mario =
  let
    dt = 2
    vx =
      let keyX = (toX computer.keyboard) in
        if keyX /= 0 then keyX * runSpeed else (mario.vx * coast)

    gravityApplied = mario.vy - dt * gravity
    vy = 
      if mario.y == 0 && computer.keyboard.up then  -- on ground, new jump starts
        jumpPower
      else if computer.keyboard.up then  -- in air, holding jump key for long jump
        gravityApplied
      else
        min jumpCutoff gravityApplied  -- jump key released, limit speed to allow var height jumps

    newX = mario.x + dt * vx
    newY = max 0 (mario.y + dt * vy)

    resetIfInLava = isInLava newX newY
    teleportIfInPortal = isInPortal newX newY

    inverseX = -mario.x

  in
    if resetIfInLava then
      initialState
    else if teleportIfInPortal then
      { mario
        | x = inverseX
        , y = 1000 
        , vy = 0  
      }
    else
    { mario
      | x = newX
      , y = newY
      , vx = vx
      , vy = (newY - mario.y) / dt
      , dir =
          if (toX computer.keyboard) < 0 then
            Left
          else if (toX computer.keyboard) > 0 then
            Right
          else
            mario.dir  -- face direction of last movement when standing still
      , trace = addPointUnlessDuplicate (newX, newY) mario.trace
    }

addPointUnlessDuplicate point path =
  if (List.head path) == Just point then
    path
  else
    point :: path

-- HELPERS

-- Elm’s playground package doesn't have any way to stroke a path.
-- This function makes a polygon that traces across the given points
-- offset slightly, then traces in reverse order with the opposite offset.
pathToPolygonVertices thickness path =
  (path |> offsetPath (thickness, -thickness))
  ++
  (List.reverse path |> offsetPath (-thickness, thickness))

offsetPath offset points =
    List.map (pointAdd offset) points

pointAdd (x0, y0) (x1, y1) =
  (x0 + x1, y0 + y1)
