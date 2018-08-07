module Helpers exposing (..)

import Point2 exposing (Point2(..))
import Model exposing (..)


sign : number -> number
sign value =
    if (value + 0) > 0 then
        1
    else if value < 0 then
        -1
    else
        0


ifThenElse : Bool -> a -> a -> a
ifThenElse condition ifTrue ifFalse =
    if condition then
        ifTrue
    else
        ifFalse


tupleCombine : (a -> b -> c) -> ( a, b ) -> c
tupleCombine combine ( a, b ) =
    combine a b


absoluteStyle : Point2 ViewCoord number -> Point2 ViewCoord number2 -> List ( String, String )
absoluteStyle (Point2 position) (Point2 size) =
    [ ( "position", "absolute" )
    , ( "left", px position.x )
    , ( "top", px position.y )
    , ( "width", px size.x )
    , ( "height", px size.y )
    ]


px : number -> String
px value =
    toString value ++ "px"


logMapped : String -> (a -> String) -> a -> a
logMapped tag map value =
    let
        _ =
            map value |> Debug.log tag
    in
        value
