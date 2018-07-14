module Model exposing (..)

import Array2 exposing (Array2)
import Point2 exposing (Point2)


type BlockCoord
    = BlockCoord


type GridCoord
    = GridCoord


type WorldCoord
    = WorldCoord


type ViewCoord
    = ViewCoord


type alias Model =
    { grid : Array2 GridCoord GridCell
    , fullSize : Point2 WorldCoord Int
    , blocks : List Block
    , gridOffset : Point2 WorldCoord Int
    , gameStarted : Bool
    }


type alias GridRecord a =
    { a
        | grid : Array2 GridCoord GridCell
        , gridOffset : Point2 WorldCoord Int
    }


type GridCell
    = Empty
    | BlockCell


type alias Block =
    { blocks : List (Point2 BlockCoord Int)
    , rotation : Int
    , position : Point2 WorldCoord Int
    , direction : Direction
    }


type alias Direction =
    Int


type PortOutMsg
    = PlaySound { soundName : String, loop : Bool }
    | StopSound { soundName : String }
