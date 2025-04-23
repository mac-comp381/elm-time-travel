module TimeTravel exposing (addTimeTravel)

import Playground exposing (..)
import Set

controlBarHeight = 64
maxVisibleHistory = 2000

addTimeTravel rawGame = 
    {initialState = initialStateWithTimeTravel rawGame
    , view = viewWithTimeTravel rawGame
    , updateState = updateWithTimeTravel rawGame
    }

initialStateWithTimeTravel rawGame =
    {rawModel = rawGame.initialState
    , paused = False
    , history = []
    , historyPlaybackPosition = 0
    }

viewWithTimeTravel rawGame computer model =
  let
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

    helpMessage =
        if model.paused then
          "Press R to resume"
        else
          "Press T to time travel"
  in
    (rawGame.view computer model.rawModel) ++
      [historyBar black 0.3 maxVisibleHistory] ++
      [historyBar red 0.6 (List.length model.history)] ++
      [historyBar blue 0.6 model.historyPlaybackPosition] ++
      [words black helpMessage
          |> move 0 (computer.screen.top - controlBarHeight / 2)
      ]

updateWithTimeTravel rawGame computer model =
    if model.paused && computer.mouse.down then
        let
            historyPlaybackPosition = min (mousePosToHistoryIndex computer) (List.length model.history)
            replayHistory pastInputs = List.foldl rawGame.updateState rawGame.initialState pastInputs
        in 
            {model | historyPlaybackPosition = historyPlaybackPosition
            , rawModel = replayHistory (List.take historyPlaybackPosition model.history)}
    else if model.paused then
        if keyPressed "T" computer then
            {model | paused = True}
        else if keyPressed "R" computer then
            {model | paused = False
            , history = List.take model.historyPlaybackPosition model.history
            }
        else
            model

    else
        let
            rawModel = rawGame.updateState computer model.rawModel
            paused =
                if keyPressed "T" computer then
                    True
                else if keyPressed "R" computer then
                    False
                else 
                    model.paused
            history = model.history ++ [computer]
            historyPlaybackPosition = (List.length model.history) + 1
        in
            {model | rawModel = rawModel
            , paused = paused
            , history = history
            , historyPlaybackPosition = historyPlaybackPosition
            }
    

  
  

-- HELPER FUNCTIONS --

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