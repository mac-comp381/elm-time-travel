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

viewWithTimeTravel rawGame computer model =
    let
        helpMessage = 
            if model.paused then
                "press R to resume"
            else
                "Press T to time travel"
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
            , historyBar (rgb 0 0 255) 0.6 (List.length model.history)
            , historyBar (rgb 0 255 0) 0.6 model.historyPlaybackPosition
            , words white helpMessage
                |> move 0 (computer.screen.top - controlBarHeight / 2)    
            ]

updateWithTimeTravel rawGame computer model =
    let
        newPaused = if keyPressed "T" computer then
                True
            else if keyPressed "R" computer then
                False
            else
                model.paused
        newHistory =  if keyPressed "R" computer then
                List.take model.historyPlaybackPosition model.history
            else if model.paused then
                model.history
            else
                model.history ++ [computer]
    in
    if model.paused && computer.mouse.down then
        let
            newPlaybackPosition = min (mousePosToHistoryIndex computer) (List.length model.history)
            replayHistory pastInputs =
                List.foldl rawGame.updateState rawGame.initialState pastInputs
        in
        {model 
        | historyPlaybackPosition = newPlaybackPosition
        , rawModel = replayHistory (List.take newPlaybackPosition model.history)
        }
    else if model.paused then
        {model | paused = newPaused, history = newHistory}
    else
        { model 
        | rawModel = rawGame.updateState computer model.rawModel
        , paused = newPaused
        , history = newHistory
        , historyPlaybackPosition = List.length model.history + 1
        }


keyPressed keyName computer =
    [ String.toLower keyName
    , String.toUpper keyName
    ]
        |> List.any (\key -> Set.member key computer.keyboard.keys)

historyIndexToX computer index =
    (toFloat index) / maxVisibleHistory * computer.screen.width

mousePosToHistoryIndex computer =
    (computer.mouse.x - computer.screen.left)
        / computer.screen.width * maxVisibleHistory
    |> round