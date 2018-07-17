module Tests exposing (..)

import Array2
import Block
import Converters
import Expect
import Main
import Model exposing (..)
import Point2 exposing (Point2)
import Set
import Test exposing (..)
import Point2


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
        , test "Get filled lines" <|
            \_ ->
                let
                    grid =
                        Array2.init (Point2.new 3 2) Empty
                            |> Array2.set (Point2.new 0 0) BlockCell
                            |> Array2.set (Point2.new 2 0) BlockCell
                            |> Array2.set (Point2.new 0 1) BlockCell
                            |> Array2.set (Point2.new 1 1) BlockCell
                            |> Array2.set (Point2.new 2 1) BlockCell
                in
                    grid
                        |> Main.getFilledLines
                        |> Expect.equal { rows = Set.fromList [ 1 ], columns = Set.fromList [ 0, 2 ] }
        ]
