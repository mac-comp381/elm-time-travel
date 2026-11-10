module TimeTravel exposing (addTimeTravel)

import Playground exposing (..)
import Set

controlBarHeight = 64
maxVisibleHistory = 2000

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

updateWithTimeTravel rawGame computer currentModel =
  let
    updatedModel = 
      { currentModel
      | rawModel = rawGame.updateState computer currentModel.rawModel
      , history = currentModel.history ++ [computer]
      , historyPlaybackPosition = currentModel.historyPlaybackPosition + 1
      }
    newPlaybackPosition = 
      min
        (mousePosToHistoryIndex computer)
        (List.length currentModel.history)
    replayHistory pastInputs = 
      List.foldl 
        (\input model -> rawGame.updateState input model)
        rawGame.initialState
        pastInputs
  in
    if currentModel.paused && computer.mouse.down then
      { currentModel
      | historyPlaybackPosition = newPlaybackPosition
      , rawModel = replayHistory (List.take newPlaybackPosition currentModel.history)
      }
    else if keyPressed "R" computer then
      { updatedModel
      | paused = False
      , history = List.take currentModel.historyPlaybackPosition currentModel.history
      }
    else if keyPressed "T" computer then
      { currentModel | paused = True }
    else if keyPressed "C" computer then
      { currentModel
      | rawModel = rawGame.initialState
      , history = []
      , historyPlaybackPosition = 0
      }
    else if currentModel.paused then
      currentModel
    else
      updatedModel

viewWithTimeTravel rawGame computer model =
  let
    helpMessage =
      if model.paused then
        [ words white "Press R to resume or C to reset"
          |> move 0 (computer.screen.top - controlBarHeight / 2 + 10)
        , words white "Drag the bar to move back and forth in time"
          |> move 0 (computer.screen.top - controlBarHeight / 2 - 10)
        ]
      else
        [ words white "Press T to time travel or C to reset"
          |> move 0 (computer.screen.top - controlBarHeight / 2)]
    -- Creates a rectangle at the top of the screen, stretching from the
    -- left edge up to a specific position within the history timeline
    historyBar color opacity index =
      let
        width = historyIndexToX computer index
      in
        rectangle color width controlBarHeight  
          |> move (computer.screen.left + width / 2)
                  (computer.screen.top - controlBarHeight / 2)
          |> fade opacity
  in
    (rawGame.view computer model.rawModel) ++
      [ historyBar black 0.3 maxVisibleHistory
      , historyBar (rgb 129 61 156) 0.6 (List.length model.history)
      , historyBar green 0.6 model.historyPlaybackPosition
      ] ++ helpMessage

keyPressed keyName computer =
  [ String.toLower keyName
  , String.toUpper keyName
  ]
    |> List.any (\key -> Set.member key computer.keyboard.keys)

-- Converts an index in the history list to an x coordinate on the screen
historyIndexToX computer index =
  (toFloat index) / maxVisibleHistory * computer.screen.width

-- Converts the mouse's current position to an index within the history list
mousePosToHistoryIndex computer =
  (computer.mouse.x - computer.screen.left)
    / computer.screen.width * maxVisibleHistory
  |> round
