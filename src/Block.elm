module Block exposing (..)

import Point2 exposing (Point2(..))
import Model exposing (..)


move : Block -> Block
move blockGroup =
    { blockGroup
        | position =
            Point2 { x = 1, y = 0 }
                |> Point2.rotateBy90 blockGroup.direction
                |> Point2.add blockGroup.position
    }


blockLocalToWorld : Block -> Point2 BlockCoord Int -> Point2 WorldCoord Int
blockLocalToWorld blockGroup (Point2 localPosition) =
    Point2.add blockGroup.position (Point2 localPosition)


square : List (Point2 a Int)
square =
    [ Point2.new 0 0
    , Point2.new 0 1
    , Point2.new 1 0
    , Point2.new 1 1
    ]


straight : List (Point2 a Int)
straight =
    [ Point2.new 0 -1
    , Point2.new 0 0
    , Point2.new 0 1
    , Point2.new 0 2
    ]


pyramid : List (Point2 a Int)
pyramid =
    [ Point2.new 0 -1
    , Point2.new 0 0
    , Point2.new 0 1
    , Point2.new -1 0
    ]


hook : List (Point2 a Int)
hook =
    [ Point2.new 0 -1
    , Point2.new 0 0
    , Point2.new 0 1
    , Point2.new -1 1
    ]


hookMirrored : List (Point2 a Int)
hookMirrored =
    [ Point2.new 0 -1
    , Point2.new 0 0
    , Point2.new 0 1
    , Point2.new 1 1
    ]
