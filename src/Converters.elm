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
blockLocalToWorld block localPosition =
    let
        offset =
            if block.rotationHalfOffset then
                Point2.new 0.5 0.5
            else
                Point2.zero
    in
        localPosition
            |> Point2.map toFloat
            |> Point2.add offset
            |> Point2.rotateBy90 block.rotation
            |> flip Point2.sub offset
            |> Point2.map round
            |> Point2.unsafeConvert
            |> Point2.add block.position
