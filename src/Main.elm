module Main exposing (..)

import Mario
import Asteroids
import TimeTravel exposing (addTimeTravel)


import Playground

-- Converts the record-based { view, initialState, updateState } games this project uses into
-- an application that Elm knows how to run.
--
gameApplication game =
  Playground.game game.view game.updateState game.initialState

-- The main entry point for the app

main = Mario.game
  |> addTimeTravel
  |> gameApplication
