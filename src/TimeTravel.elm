module TimeTravel exposing (addTimeTravel)

import Playground exposing (..)
import Set


controlBarHeight = 64

main = Mario.game  -- or Asteroids.game, either should work
  |> addTimeTravel
  |> gameApplication

addTimeTravel rawGame =
  { initialState = initialStateWithTimeTravel rawGame
  , updateState = updateWithTimeTravel rawGame
  , view = viewWithTimeTravel rawGame
  }

initialStateWithTimeTravel rawGame =
  { rawModel = rawGame.initialState
  , paused = False
  , history = []
  }

viewWithTimeTravel rawGame computer model =
  let
    historyBar color opacity index =
      let
        width = historyIndexToX computer index
      in
        rectangle color width controlBarHeight  
          |> move (computer.screen.left + width / 2)
                  (computer.screen.top - controlBarHeight / 2)
          |> fade opacity
    helpMessage =
        if model.paused then
          "Press R to resume"
        else
          "Press T to time travel"
    in
    (rawGame.view computer model.rawModel) ++
      -- Add two history bars: one black background and one colorful progress bar
      [ historyBar (rgb 0 0 0) 0.3 (List.length model.history)
      , historyBar (rgb 0 0 255) 0.6 (List.length model.history)
      , words white helpMessage
          |> move 0 (computer.screen.top - controlBarHeight / 2)
      ]

updateWithTimeTravel rawGame computer model =
  if ...
    ...
  else
    { model
      | rawModel = rawGame.updateState computer model.rawModel
      , history = model.history ++ [computer]
    }

keyPressed keyName computer =
  [ String.toLower keyName
  , String.toUpper keyName
  ]
    |> List.any (\key -> Set.member key computer.keyboard.keys)

maxVisibleHistory = 2000

-- Converts an index in the history list to an x coordinate on the screen
historyIndexToX computer index =
  (toFloat index) / maxVisibleHistory * computer.screen.width

-- Converts the mouse's current position to an index within the history list
mousePosToHistoryIndex computer =
  (computer.mouse.x - computer.screen.left)
    / computer.screen.width * maxVisibleHistory
  |> round