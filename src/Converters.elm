module Converters exposing (..)

import Model exposing (..)
import Point2 exposing (Point2(..))


worldToGrid : GridRecord a -> Point2 WorldCoord Int -> Point2 GridCoord Int
worldToGrid model point =
    Point2.sub point model.gridOffset |> Point2.unsafeConvert


gridToWorld : Point2 WorldCoord number -> Point2 GridCoord number -> Point2 WorldCoord number
gridToWorld gridOffset point =
    point |> Point2.unsafeConvert |> Point2.add gridOffset


blockToWorld : Block -> List (Point2 WorldCoord Int)
blockToWorld block =
    let
        offset =
            if block.rotationHalfOffset then
                Point2.zero
            else
                Point2.new 0.5 0.5
    in
        block.blocks
            |> List.map
                (Point2.map toFloat
                    >> Point2.add offset
                    >> Point2.rotateBy90 block.rotation
                    >> flip Point2.sub offset
                    >> Point2.map round
                    >> Point2.unsafeConvert
                    >> Point2.add block.position
                )
