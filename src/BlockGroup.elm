module BlockGroup exposing (..)

import Point2 exposing (Point2)


type alias BlockGroup =
    { blocks : List (Point2 Int)
    , rotation : Int
    , position : Point2 Int
    , direction : Int
    }


move : BlockGroup -> BlockGroup
move blockGroup =
    { blockGroup
        | position =
            Point2 1 0
                |> Point2.rotateBy90 blockGroup.direction
                |> Point2.add blockGroup.position
    }


getBlocksWorldCoord : BlockGroup -> List (Point2 Int)
getBlocksWorldCoord blockGroup =
    blockGroup.blocks |> List.map (Point2.add blockGroup.position)


square : List (Point2 Int)
square =
    [ Point2 0 0, Point2 0 1, Point2 1 0, Point2 1 1 ]


straight : List (Point2 Int)
straight =
    [ Point2 0 -1, Point2 0 0, Point2 0 1, Point2 0 2 ]


pyramid : List (Point2 Int)
pyramid =
    [ Point2 0 -1, Point2 0 0, Point2 0 1, Point2 -1 0 ]


hook : List (Point2 Int)
hook =
    [ Point2 0 -1, Point2 0 0, Point2 0 1, Point2 -1 1 ]


hookMirrored : List (Point2 Int)
hookMirrored =
    [ Point2 0 -1, Point2 0 0, Point2 0 1, Point2 1 1 ]
