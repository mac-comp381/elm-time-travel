module TimeTravel exposing (addTimeTravel)

import Playground exposing (..)
import Set

-- Constants --
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
    , rgb = {r = 0, g = 0, b = 0}
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
            "Drag bar to time travel or Press R to resume"
            else
            "Press T to time travel or C to reset game"
    in
        (rawGame.view computer model.rawModel) ++
        [ historyBar black 0.3 maxVisibleHistory
        , historyBar white 0.6 (List.length model.history)
        , historyBar (rgb model.rgb.r model.rgb.g model.rgb.b) 1 model.historyPlaybackPosition
        , words white helpMessage
            |> move 0 (computer.screen.top - controlBarHeight / 2)
        ]
        

updateWithTimeTravel rawGame computer model = 
    if keyPressed "C" computer then
        initialStateWithTimeTravel rawGame
    else if model.paused && computer.mouse.down then
        let 
            newPlaybackPosition = 
                min (mousePosToHistoryIndex computer) (List.length model.history)
            replayHistory pastInputs =
                List.foldl rawGame.updateState rawGame.initialState pastInputs
        in 
            { model 
            | historyPlaybackPosition = newPlaybackPosition
            , rawModel = replayHistory (List.take newPlaybackPosition model.history )
            }
    else if keyPressed "T" computer then
        { model | paused = True}
    else if model.paused && keyPressed "R" computer then
        {model 
        | paused = False
        , history = (List.take model.historyPlaybackPosition model.history)
        }
    else if model.paused then
        model
    else 
        { model 
        | rawModel = rawGame.updateState computer model.rawModel
        , history = model.history ++ [computer] 
        , historyPlaybackPosition = (List.length model.history) + 1
        , rgb = calculateNewRainbowColor model.rgb
        }

-- Helper Functions --

-- Returns true if key is currently being pressed
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

dr = 3
dg = 1
db = 2
-- Calculates next color to change to in a rainbow animation
calculateNewRainbowColor {r, g, b} = 
    { r = wrapColorValue (r + dr)
    , g = wrapColorValue (g + dg)
    , b = wrapColorValue (b + db)
    }

-- Wraps color channel values back around when they are over 255
wrapColorValue val =
    let 
        overflow = val - 255
    in 
        if overflow > 0 then
            overflow
        else 
            val

