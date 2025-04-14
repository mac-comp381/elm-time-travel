module TimeTravel exposing (addTimeTravel)

import Playground exposing (..)
import Set

controlBarHeight = 64

maxVisibleHistory = 2000

-- Converts an index in the history list to an x coordinate on the screen
historyIndexToX computer index =
  (toFloat index) / maxVisibleHistory * computer.screen.width
-- Converts the mouse's current position to an index within the history list
mousePosToHistoryIndex computer =
  (computer.mouse.x - computer.screen.left)
    / computer.screen.width * maxVisibleHistory
  |> round

addTimeTravel rawGame =
  { initialState = initialStateWithTimeTravel rawGame
  , updateState = updateWithTimeTravel rawGame
  , view = viewWithTimeTravel rawGame
  }

initialStateWithTimeTravel rawGame =
  { rawModel = rawGame.initialState
  , paused = False
  , history = []
  , historyPlaybackPosition = 0 
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
      [ historyBar black 0.3  maxVisibleHistory
      , historyBar (rgb 0 0 255) 0.6 (List.length model.history)
      , historyBar (rgb 255 255 0) 0.6 model.historyPlaybackPosition
      , words white helpMessage
          |> scale 3
          |> move 0 (computer.screen.top - controlBarHeight / 2)
      ]

updateWithTimeTravel rawGame computer model =

  if model.paused && computer.mouse.down then
    let

      newPlaybackPosition = min ( mousePosToHistoryIndex computer)  (List.length model.history)
      replayHistory pastInputs=
        List.foldl( \input state -> rawGame.updateState input state) rawGame.initialState pastInputs
    in

      { model
        | historyPlaybackPosition = newPlaybackPosition
        , rawModel = replayHistory (List.take newPlaybackPosition model.history)
      }
  --check for t and r input
  else if keyPressed "T" computer then
    { model | paused = True }
  else if keyPressed "R" computer then 
    { model | paused =  False 
    , history = List.take model.historyPlaybackPosition model.history
    , historyPlaybackPosition = List.length(List.take model.historyPlaybackPosition model.history)
    }
  else if model.paused == True then
    model
  else
    { model 
    | rawModel = rawGame.updateState computer model.rawModel 
    , history = model.history ++ [computer]
    , historyPlaybackPosition = List.length model.history + 1
    }

keyPressed keyName computer =
  [ String.toLower keyName
  , String.toUpper keyName
  ]
    |> List.any (\key -> Set.member key computer.keyboard.keys)
