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
                            1
                            (Point2.new 5 5)
                            0

                    baseModel =
                        { grid =
                            Array2.init
                                (Point2.new 25 25)
                                Empty
                                |> Array2.set (Point2.new 5 3) BlockCell
                        , blocks = [ block ]
                        , gridOffset = Point2.zero
                        }

                    result =
                        baseModel |> Main.moveCrosshair 3

                    expected =
                        { baseModel
                            | gridOffset = Point2.new 0 1
                            , blocks = [ { block | position = Point2.new 5 6 } ]
                        }
                in
                    -- we don't compare the models directly cause the test output for Array2 is really noisey
                    result
                        |> Expect.all
                            [ .gridOffset >> Expect.equal expected.gridOffset
                            , .blocks >> Expect.equalLists expected.blocks
                            ]
        ]
