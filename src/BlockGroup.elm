module BlockGroup exposing (..)

import Point2 exposing (Point2(..))
import Model exposing (..)


move : BlockGroup -> BlockGroup
move blockGroup =
    { blockGroup
        | position =
            Point2 { x = 1, y = 0 }
                |> Point2.rotateBy90 blockGroup.direction
                |> Point2.add blockGroup.position
    }


blockLocalToWorld : BlockGroup -> Point2 BlockCoord Int -> Point2 WorldCoord Int
blockLocalToWorld blockGroup (Point2 localPosition) =
    Point2.add blockGroup.position (Point2 localPosition)


square : List (Point2 a Int)
square =
    [ { x = 0, y = 0 }
    , { x = 0, y = 1 }
    , { x = 1, y = 0 }
    , { x = 1, y = 1 }
    ]
        |> List.map Point2


straight : List (Point2 a Int)
straight =
    [ { x = 0, y = -1 }
    , { x = 0, y = 0 }
    , { x = 0, y = 1 }
    , { x = 0, y = 2 }
    ]
        |> List.map Point2


pyramid : List (Point2 a Int)
pyramid =
    [ { x = 0, y = -1 }
    , { x = 0, y = 0 }
    , { x = 0, y = 1 }
    , { x = -1, y = 0 }
    ]
        |> List.map Point2


hook : List (Point2 a Int)
hook =
    [ { x = 0, y = -1 }
    , { x = 0, y = 0 }
    , { x = 0, y = 1 }
    , { x = -1, y = 1 }
    ]
        |> List.map Point2


hookMirrored : List (Point2 a Int)
hookMirrored =
    [ { x = 0, y = -1 }
    , { x = 0, y = 0 }
    , { x = 0, y = 1 }
    , { x = 1, y = 1 }
    ]
        |> List.map Point2
