module Array2Tests exposing (..)

import Array2
import Expect
import Fuzz
import Point2 exposing (Point2(..))
import Test exposing (..)


tests : Test
tests =
    describe "A Test Suite"
        [ fuzz2 (Fuzz.intRange 0 5) (Fuzz.intRange 0 5) "Array.toIndexedList" <|
            \x y ->
                let
                    point =
                        Point2.new x y
                in
                    Array2.init (Point2.new 3 4) False
                        |> Array2.set point True
                        |> Array2.toIndexedList
                        |> List.all (\( p, v ) -> (p == point) == v)
                        |> Expect.true "Only the point we set should be true."
        , only <|
            test "toString test" <|
                \_ ->
                    let
                        array2 =
                            Array2.init (Point2.new 2 1) ' '
                    in
                        array2
                            |> Array2.toString identity
                            |> Expect.equal "  \n"
        , only <|
            test "fromString test" <|
                \_ ->
                    let
                        expected =
                            Array2.init (Point2.new 2 1) ' '
                    in
                        expected
                            |> Array2.toString identity
                            |> Debug.log ""
                            |> Array2.fromString identity ' '
                            |> Expect.equal (Array2.init (Point2.new 2 1) ' ')
        , fuzz3
            (Fuzz.intRange 1 5)
            (Fuzz.intRange 1 5)
            (Fuzz.list
                (Fuzz.char
                    |> Fuzz.map
                        (\a ->
                            if a == '\n' || a == '\x0D' then
                                ' '
                            else
                                a
                        )
                )
            )
            "fromString undoes toString for non-zero sized arrays"
          <|
            \x y values ->
                let
                    defaultValue =
                        ' '

                    array2 =
                        List.foldl
                            (\( index, value ) array2 ->
                                Array2.set (Point2.intToInt2 x index) value array2
                            )
                            (Array2.init (Point2.new x y) defaultValue)
                            (List.indexedMap (,) values)
                in
                    array2
                        |> Array2.toString identity
                        --|> Debug.log (values |> String.fromList)
                        |> Array2.fromString identity defaultValue
                        |> Expect.equal array2
        ]
