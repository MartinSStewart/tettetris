module BlockGroup exposing (..)

import TypedPoint2 exposing (TypedPoint2(..))
import Point2 exposing (Point2)
import Model exposing (..)


move : BlockGroup -> BlockGroup
move blockGroup =
    { blockGroup
        | position =
            TypedPoint2 { x = 1, y = 0 }
                |> TypedPoint2.rotateBy90 blockGroup.direction
                |> TypedPoint2.add blockGroup.position
    }


blockLocalToWorld : BlockGroup -> TypedPoint2 BlockLocalPosition Int -> TypedPoint2 WorldPosition Int
blockLocalToWorld blockGroup (TypedPoint2 localPosition) =
    TypedPoint2.add blockGroup.position (TypedPoint2 localPosition)


square : List (TypedPoint2 a Int)
square =
    [ Point2 0 0, Point2 0 1, Point2 1 0, Point2 1 1 ] |> List.map TypedPoint2


straight : List (TypedPoint2 a Int)
straight =
    [ Point2 0 -1, Point2 0 0, Point2 0 1, Point2 0 2 ] |> List.map TypedPoint2


pyramid : List (TypedPoint2 a Int)
pyramid =
    [ Point2 0 -1, Point2 0 0, Point2 0 1, Point2 -1 0 ] |> List.map TypedPoint2


hook : List (TypedPoint2 a Int)
hook =
    [ Point2 0 -1, Point2 0 0, Point2 0 1, Point2 -1 1 ] |> List.map TypedPoint2


hookMirrored : List (TypedPoint2 a Int)
hookMirrored =
    [ Point2 0 -1, Point2 0 0, Point2 0 1, Point2 1 1 ] |> List.map TypedPoint2
