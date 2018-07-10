module Array2 exposing (..)

import Array.Hamt as Array
import Point2 exposing (Point2)


type alias Array2 a =
    { size : Point2 Int, data : Array.Array (Array.Array a) }


init : Point2 Int -> a -> Array2 a
init size value =
    { size = size, data = Array.repeat size.x (Array.repeat size.y value) }


get : Point2 Int -> Array2 a -> Maybe a
get point array2 =
    Array.get point.x array2.data |> Maybe.andThen (Array.get point.y)


set : Point2 Int -> a -> Array2 a -> Array2 a
set point value array2 =
    let
        column =
            Array.get point.x array2.data

        newData =
            case column of
                Just a ->
                    Array.set point.x (Array.set point.y value a) array2.data

                Nothing ->
                    array2.data
    in
        { size = array2.size, data = newData }


replace : Point2 Int -> (a -> a) -> Array2 a -> Array2 a
replace point replaceFunc array2 =
    case get point array2 of
        Just a ->
            set point (replaceFunc a) array2

        Nothing ->
            array2


toIndexedList : Array2 a -> List ( Point2 Int, a )
toIndexedList array2 =
    let
        arrayToIndexList =
            Array.toList >> List.indexedMap (\index value -> ( index, value ))
    in
        arrayToIndexList array2.data
            |> List.concatMap
                (\( x, a ) ->
                    arrayToIndexList a
                        |> List.map (\( y, b ) -> ( { x = x, y = y }, b ))
                )


size : Array2 a -> Point2 Int
size array2 =
    array2.size
