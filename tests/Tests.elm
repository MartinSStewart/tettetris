module Tests exposing (..)

import Array2
import Block
import Converters
import Expect
import Grid exposing (Bands)
import Model exposing (..)
import Point2 exposing (Point2(..))
import Set
import Test exposing (..)


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
                    Grid.collides model block
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
                            |> Grid.moveCrosshair 3
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
                            |> Grid.moveCrosshair 2
                in
                    result
                        |> Expect.all
                            [ .gridOffset >> Expect.equal (Point2.new -1 0)
                            , .blocks >> Expect.equalLists []
                            ]
        , test "Get filled rows and columns" <|
            \_ ->
                let
                    arraySize =
                        Grid.detectorMargin * 2 + 2
                in
                    Array2.init (Point2.new arraySize arraySize) Empty
                        |> Array2.map
                            (\( Point2 a, b ) ->
                                if a.x == 0 || a.x == 1 then
                                    BlockCell
                                else
                                    Empty
                            )
                        |> Grid.filledLines BlockCell
                        |> Expect.equal
                            { rows = Set.empty
                            , columns = [ 0, 1 ] |> Set.fromList
                            }
        , test "Get empty rows and columns" <|
            \_ ->
                let
                    arraySize =
                        Grid.detectorMargin * 2 + 2
                in
                    Array2.init (Point2.new arraySize arraySize) Empty
                        |> Grid.filledLines Empty
                        |> Expect.equal
                            { rows = List.range 0 (arraySize - 1) |> Set.fromList
                            , columns = List.range 0 (arraySize - 1) |> Set.fromList
                            }
        , test "Compactify grid" <|
            \_ ->
                let
                    arraySize =
                        4

                    grid =
                        Array2.init (Point2.new arraySize arraySize) Empty
                            |> Array2.set (Point2.new 2 1) BlockCell
                            |> Array2.set (Point2.new 3 3) BlockCell

                    expected =
                        Array2.init (Point2.new arraySize arraySize) Empty
                            |> Array2.set (Point2.new 2 1) BlockCell

                    charSelector gridCell =
                        case gridCell of
                            BlockCell ->
                                'x'

                            Empty ->
                                '.'
                in
                    Grid.compactify (Bands (Set.fromList [ 3 ]) (Set.fromList [])) grid
                        |> Array2.toString charSelector
                        |> Expect.equal (expected |> Array2.toString charSelector)
        ]
