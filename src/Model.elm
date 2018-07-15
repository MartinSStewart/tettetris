module Model exposing (..)

import Array2 exposing (Array2)
import Point2 exposing (Point2)
import Random


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
    , blocks : List Block
    , gridOffset : Point2 WorldCoord Int
    , gameStarted : Bool
    , randomSeed : Random.Seed
    , newBlockCountdown : Int
    }


type alias Flags =
    { initialSeed : Float }


type alias GridRecord a =
    { a
        | grid : Array2 GridCoord GridCell
        , gridOffset : Point2 WorldCoord Int
    }


type alias BlockRecord a =
    { a
        | blocks : List Block
    }


type GridCell
    = Empty
    | BlockCell


type alias Block =
    { blocks : List (Point2 BlockCoord Int)
    , rotationHalfOffset : Bool
    , rotation : Int
    , position : Point2 WorldCoord Int
    , direction : Direction
    }


type alias Direction =
    Int


type PortOutMsg
    = PlaySound { soundName : String, loop : Bool }
    | StopSound { soundName : String }
