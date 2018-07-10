module Model exposing (..)

import Array2 exposing (Array2)
import TypedPoint2 exposing (TypedPoint2)


type BlockLocalPosition
    = BlockLocalPosition


type GridLocalPosition
    = GridLocalPosition


type WorldPosition
    = WorldPosition


type ViewPosition
    = ViewPosition


type alias Model =
    { grid : Array2 GridCell
    , fullSize : TypedPoint2 WorldPosition Int
    , blockGroups : List BlockGroup
    , gridOffset : TypedPoint2 WorldPosition Int
    }


type GridCell
    = Empty
    | BlockCell


type alias BlockGroup =
    { blocks : List (TypedPoint2 BlockLocalPosition Int)
    , rotation : Int
    , position : TypedPoint2 WorldPosition Int
    , direction : Int
    }
