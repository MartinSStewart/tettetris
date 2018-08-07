module Array2 exposing (..)

import Array as Array exposing (Array)
import Point2 exposing (Point2(..))


type alias Array2 a b =
    { size : Point2 a Int, data : Array (Array b) }


init : Point2 a Int -> b -> Array2 a b
init (Point2 size) value =
    { size = Point2 size, data = Array.repeat size.x (Array.repeat size.y value) }


get : Point2 a Int -> Array2 a b -> Maybe b
get (Point2 point) array2 =
    Array.get point.x array2.data |> Maybe.andThen (Array.get point.y)


set : Point2 a Int -> b -> Array2 a b -> Array2 a b
set (Point2 point) value array2 =
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


map : (( Point2 a Int, b ) -> b) -> Array2 a b -> Array2 a b
map mapFunc array2 =
    toIndexedList array2
        |> List.foldl
            (\( pos, value ) b -> b |> set pos (mapFunc ( pos, value )))
            array2


replace : Point2 a Int -> (b -> b) -> Array2 a b -> Array2 a b
replace point replaceFunc array2 =
    case get point array2 of
        Just a ->
            set point (replaceFunc a) array2

        Nothing ->
            array2


toIndexedList : Array2 a b -> List ( Point2 a Int, b )
toIndexedList array2 =
    let
        arrayToIndexList =
            Array.toList >> List.indexedMap (\index value -> ( index, value ))
    in
        arrayToIndexList array2.data
            |> List.concatMap
                (\( x, a ) ->
                    arrayToIndexList a
                        |> List.map (\( y, b ) -> ( Point2 { x = x, y = y }, b ))
                )


size : Array2 a b -> Point2 a Int
size array2 =
    array2.size


toString : (b -> Char) -> Array2 a b -> String
toString charSelector array2 =
    let
        (Point2 size) =
            array2.size
    in
        List.range 0 (size.x - 1)
            |> List.foldl
                (\x text ->
                    List.range 0 (size.x - 1)
                        |> List.foldl
                            (\y text ->
                                array2
                                    |> get (Point2.new x y)
                                    |> Maybe.map charSelector
                                    |> Maybe.withDefault ' '
                                    |> flip (::) text
                            )
                            [ '\n' ]
                        |> String.fromList
                        |> flip (::) text
                )
                []
            |> String.join ""



-- toIndexedList array2
--     |> List.sortWith
--         (\( Point2 pos0, _ ) ( Point2 pos1, _ ) ->
--             if pos0.y < pos1.y then
--                 LT
--             else if pos0.y > pos1.y then
--                 GT
--             else if pos0.x < pos1.x then
--                 LT
--             else if pos0.x > pos1.x then
--                 GT
--             else
--                 EQ
--         )
--     |> List.map (\( pos, value ) -> charSelector value) |>


fromString : (Char -> b) -> b -> String -> Array2 a b
fromString cellSelector defaultValue text =
    let
        textLines =
            String.filter ((/=) '\x0D') text
                |> String.split "\n"

        size =
            Point2.new
                (textLines |> List.map String.length |> List.maximum |> Maybe.withDefault 0)
                (textLines |> List.length |> (+) -1)
    in
        textLines
            |> List.foldl
                (\textLine ( y, array2 ) ->
                    List.foldl
                        (\char ( x, array2 ) ->
                            set (Point2.new x y) (cellSelector char) array2 |> (,) (x + 1)
                        )
                        ( 0, array2 )
                        (String.toList textLine)
                        |> Tuple.second
                        |> (,) (y + 1)
                )
                ( 0, init size defaultValue )
            |> Tuple.second
