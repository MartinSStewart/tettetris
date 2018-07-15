module Block exposing (..)

import Point2 exposing (Point2(..))
import Model exposing (..)


move : Direction -> Block -> Block
move direction blockGroup =
    { blockGroup
        | position =
            Point2 { x = 1, y = 0 }
                |> Point2.rotateBy90 direction
                |> Point2.add blockGroup.position
    }


shapes : List ( List (Point2 BlockCoord Int), Bool )
shapes =
    [ square, straight, pyramid, hook, hookMirrored ]


square : ( List (Point2 BlockCoord Int), Bool )
square =
    ( [ Point2.new -1 -1
      , Point2.new -1 0
      , Point2.new 0 -1
      , Point2.new 0 0
      ]
    , False
    )


straight : ( List (Point2 BlockCoord Int), Bool )
straight =
    ( [ Point2.new 0 -2
      , Point2.new 0 -1
      , Point2.new 0 0
      , Point2.new 0 1
      ]
    , True
    )


pyramid : ( List (Point2 BlockCoord Int), Bool )
pyramid =
    ( [ Point2.new 0 -1
      , Point2.new 0 0
      , Point2.new 0 1
      , Point2.new -1 0
      ]
    , True
    )


hook : ( List (Point2 BlockCoord Int), Bool )
hook =
    ( [ Point2.new 0 -1
      , Point2.new 0 0
      , Point2.new 0 1
      , Point2.new -1 1
      ]
    , True
    )


hookMirrored : ( List (Point2 BlockCoord Int), Bool )
hookMirrored =
    hook |> Tuple.mapFirst (List.map Point2.mirrorX)
