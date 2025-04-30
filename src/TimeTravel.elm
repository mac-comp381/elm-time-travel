module TimeTravel exposing (addTimeTravel)

import Playground exposing (..)
import Set
import List
import String


controlBarHeight = 64
maxVisibleHistory = 2000

historyIndexToX computer index =
    (toFloat index) / maxVisibleHistory * computer.screen.width

mousePosToHistoryIndex computer =
    (computer.mouse.x - computer.screen.left)
        / computer.screen.width
        * maxVisibleHistory
    |> round

keyPressed keyName computer =
    [ String.toLower keyName
    , String.toUpper keyName
    ]
        |> List.any (\key -> Set.member key computer.keyboard.keys)


type alias TimeTravelModel rawModel =
    { rawModel : rawModel
    , paused : Bool
    , history : List Computer
    , historyPlaybackPosition : Int
    }

addTimeTravel : { initialState : rawModel
                , updateState : Computer -> rawModel -> rawModel
                , view : Computer -> rawModel -> List Shape
                }
              -> { initialState : TimeTravelModel rawModel
                 , updateState : Computer -> TimeTravelModel rawModel -> TimeTravelModel rawModel
                 , view : Computer -> TimeTravelModel rawModel -> List Shape
                 }
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
              |> move (computer.screen.left + width / 2) (computer.screen.top - controlBarHeight / 2)
              |> fade opacity

        helpMessage =
            if model.paused then
                "Press R to resume; drag timeline to travel in time"
            else
                "Press T to time travel"
    in
    (rawGame.view computer model.rawModel)
        ++
        [ -- black bar representing the full timeline possible
          historyBar black 0.3 maxVisibleHistory
          -- blue bar representing how far we've progressed
        , historyBar (rgb 0 0 255) 0.6 (List.length model.history)
          -- red bar representing current playback position
        , historyBar (rgb 255 0 0) 0.8 model.historyPlaybackPosition
          -- the help text on top
        , words white helpMessage
            |> move 0 (computer.screen.top - controlBarHeight / 2)
        ]

updateWithTimeTravel rawGame computer model =
    -- We'll define a helper function to replay history:
    let
        replayHistory pastInputs =
            List.foldl
                (\input prevState -> rawGame.updateState input prevState)
                rawGame.initialState
                pastInputs

        tJustPressed = keyPressed "T" computer
        rJustPressed = keyPressed "R" computer
        cJustPressed = keyPressed "C" computer -- For optional reset

        newModel =
            if model.paused && computer.mouse.down then
                let
                    newPlaybackPosition =
                        min (mousePosToHistoryIndex computer) (List.length model.history)
                    newRawModel =
                        replayHistory (List.take newPlaybackPosition model.history)
                in
                { model
                    | historyPlaybackPosition = newPlaybackPosition
                    , rawModel = newRawModel
                }

            else if tJustPressed then
                { model | paused = True }

            else if rJustPressed then
                let
                    truncatedHistory = List.take model.historyPlaybackPosition model.history
                in
                { model
                    | paused = False
                    , history = truncatedHistory
                    , historyPlaybackPosition = (List.length truncatedHistory) + 1
                    -- After truncation, the playback position is at the end
                }

            else if cJustPressed then
                -- reset everything
                { model
                    | rawModel = rawGame.initialState
                    , history = []
                    , historyPlaybackPosition = 0
                    , paused = False
                }

            else
                model
    in
    if newModel.paused then
        newModel
    else
        let
            updatedRawModel = rawGame.updateState computer newModel.rawModel
        in
            { newModel
                | rawModel = updatedRawModel
                , history = newModel.history ++ [computer]
                , historyPlaybackPosition = (List.length newModel.history) + 1
            }
