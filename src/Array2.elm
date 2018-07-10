module Array2 exposing (..)

import Array.Hamt as Array
import TypedPoint2 exposing (TypedPoint2(..))


type alias Array2 a b =
    { size : TypedPoint2 a Int, data : Array.Array (Array.Array b) }


init : TypedPoint2 a Int -> b -> Array2 a b
init (TypedPoint2 size) value =
    { size = TypedPoint2 size, data = Array.repeat size.x (Array.repeat size.y value) }


get : TypedPoint2 a Int -> Array2 a b -> Maybe b
get (TypedPoint2 point) array2 =
    Array.get point.x array2.data |> Maybe.andThen (Array.get point.y)


set : TypedPoint2 a Int -> b -> Array2 a b -> Array2 a b
set (TypedPoint2 point) value array2 =
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


replace : TypedPoint2 a Int -> (b -> b) -> Array2 a b -> Array2 a b
replace point replaceFunc array2 =
    case get point array2 of
        Just a ->
            set point (replaceFunc a) array2

        Nothing ->
            array2


toIndexedList : Array2 a b -> List ( TypedPoint2 a Int, b )
toIndexedList array2 =
    let
        arrayToIndexList =
            Array.toList >> List.indexedMap (\index value -> ( index, value ))
    in
        arrayToIndexList array2.data
            |> List.concatMap
                (\( x, a ) ->
                    arrayToIndexList a
                        |> List.map (\( y, b ) -> ( TypedPoint2 { x = x, y = y }, b ))
                )


size : Array2 a b -> TypedPoint2 a Int
size array2 =
    array2.size
