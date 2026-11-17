module TimeTravel exposing (addTimeTravel)

import Playground exposing (..)
import Set

controlBarHeight = 64
maxVisibleHistory = 2000

historyIndexToX computer index =
  (toFloat index) / maxVisibleHistory * computer.screen.width

addTimeTravel rawGame= 
    { initialState = initialStateWithTimeTravel rawGame
    , updateState = updateWithTimeTravel rawGame
    , view = viewWithTimeTravel rawGame
    }


initialStateWithTimeTravel rawGame = {
    rawModel = rawGame.initialState
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
            "Press R to resume travel. Drag the timeline to travel through time."
        else
            "Press T to time travel"
    in
        (rawGame.view computer model.rawModel) ++
        [historyBar black 0.3 maxVisibleHistory
        , historyBar (rgb 0 0 255) 0.6 (List.length model.history)
        , historyBar (rgb 0 225 0) 0.6 (model.historyPlaybackPosition)
        , words white helpMessage
            |> move 0 (computer.screen.top - controlBarHeight / 2)
        ]
        

updateWithTimeTravel rawGame computer model =   
    if model.paused && computer.mouse.down then
    -- user can drag the process to the past moment when it's frozen.
        let
            newPlaybackPosition =
                min (mousePosToHistoryIndex computer)
                    (List.length model.history)
            replayHistory pastInputs =
                List.foldl rawGame.updateState rawGame.initialState pastInputs
        in
        -- Just like mod updating a map in Elixir
        { model
            | historyPlaybackPosition = newPlaybackPosition  -- Set the playback position to the selected history index.
            , rawModel = replayHistory (List.take newPlaybackPosition model.history) -- Take the inputs up to that position and replay them to recreate the game state.
        }

    else if keyPressed "T" computer then
        {model | paused = True}
    else if keyPressed "R" computer then
        {model | paused = False
                , history = List.take model.historyPlaybackPosition model.history}
                -- When resuming, keep history up to the selected position and discard the old history.
    else if model.paused then
        model
    else
        { model| 
        rawModel = rawGame.updateState computer model.rawModel 
        , history = model.history ++ [computer]
        , historyPlaybackPosition = (List.length model.history + 1)
        }




keyPressed keyName computer =
  [ String.toLower keyName
  , String.toUpper keyName
  ]
    |> List.any (\key -> Set.member key computer.keyboard.keys)


mousePosToHistoryIndex computer =
  (computer.mouse.x - computer.screen.left)
    / computer.screen.width * maxVisibleHistory
  |> round