module Converters exposing (..)

import Model exposing (..)
import Point2 exposing (Point2(..))


worldToGrid : Point2 WorldCoord number -> Point2 WorldCoord number -> Point2 GridCoord number
worldToGrid gridOffset point =
    Point2.sub point gridOffset |> Point2.unsafeConvert


gridToWorld : Point2 WorldCoord number -> Point2 GridCoord number -> Point2 WorldCoord number
gridToWorld gridOffset point =
    point |> Point2.unsafeConvert |> Point2.add gridOffset


blockLocalToWorld : Block -> Point2 BlockCoord Int -> Point2 WorldCoord Int
blockLocalToWorld blockGroup localPosition =
    localPosition |> Point2.unsafeConvert |> Point2.add blockGroup.position
