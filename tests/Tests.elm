module Tests exposing (..)

import Array2
import Block
import Converters
import Expect
import Main
import Model exposing (..)
import Point2 exposing (Point2(..))
import Set
import Test exposing (..)
import Point2
import Fuzz


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Collides" <|
            \_ ->
                let
                    model =
                        { grid = Array2.init (Point2.new 10 10) Empty
                        , gridOffset = Point2.new 5 5
                        , blocks = []
                        }

                    block =
                        Block
                            (Block.square |> Tuple.first)
                            (Block.square |> Tuple.second)
                            0
                            (Point2.new 0 0)
                            0
                in
                    Main.collides model block
                        |> Expect.false ""
        , test "Rotate pyramid" <|
            \_ ->
                let
                    result =
                        Block
                            (Block.pyramid |> Tuple.first)
                            (Block.pyramid |> Tuple.second)
                            1
                            Point2.zero
                            0
                            |> Converters.blockToWorld
                            |> toComparable

                    expected =
                        [ Point2.new -1 0
                        , Point2.new 0 0
                        , Point2.new 1 0
                        , Point2.new 0 1
                        ]
                            |> toComparable

                    toComparable =
                        List.map Point2.toTuple >> Set.fromList
                in
                    result |> Expect.equalSets expected
        , test "Blocks move with crosshair if not a direct collision" <|
            \_ ->
                let
                    block =
                        Block
                            (Block.square |> Tuple.first)
                            (Block.square |> Tuple.second)
                            0
                            (Point2.new 5 5)
                            0

                    result =
                        { grid =
                            Array2.init
                                (Point2.new 25 25)
                                Empty
                                |> Array2.set (Point2.new 5 3) BlockCell
                        , blocks = [ block ]
                        , gridOffset = Point2.zero
                        }
                            |> Main.moveCrosshair 3
                in
                    result
                        |> Expect.all
                            [ .gridOffset >> Expect.equal (Point2.new 0 1)
                            , .blocks >> Expect.equalLists [ { block | position = Point2.new 5 6 } ]
                            ]
        , test "Blocks are added to grid if there's a direct collision with crosshair" <|
            \_ ->
                let
                    block =
                        Block
                            (Block.square |> Tuple.first)
                            (Block.square |> Tuple.second)
                            0
                            (Point2.new 5 5)
                            0

                    result =
                        { grid =
                            Array2.init
                                (Point2.new 25 25)
                                Empty
                                |> Array2.set (Point2.new 6 5) BlockCell
                        , blocks = [ block ]
                        , gridOffset = Point2.zero
                        }
                            |> Main.moveCrosshair 2
                in
                    result
                        |> Expect.all
                            [ .gridOffset >> Expect.equal (Point2.new -1 0)
                            , .blocks >> Expect.equalLists []
                            ]
        , fuzz (Fuzz.intRange 0 5) "Set array2 row" <|
            \y ->
                Array2.init (Point2.new 3 4) False
                    |> Array2.setRow y True
                    |> Array2.toIndexedList
                    |> List.all (\( Point2 p, v ) -> (p.y == y) == v)
                    |> Expect.true ("Only values in row" ++ toString y ++ "should be true.")
        , fuzz2 (Fuzz.intRange 0 5) (Fuzz.intRange 0 5) "Array.toIndexedList" <|
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
        ]
