module Tests exposing (..)

import Test exposing (..)
import Array2
import Point2 exposing (Point2)
import Main
import Block
import Model exposing (..)
import Expect
import Converters


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

        -- , test "Rotate pyramid" <|
        --     \_ ->
        --         let
        --             result =
        --                 Block
        --                     (Block.pyramid |> Tuple.first)
        --                     (Block.pyramid |> Tuple.second)
        --                     0
        --                     Point2.zero
        --                     0
        --                     |> Converters.blockLocalToWorld
        --         in
        --             Expect.equal (Block)
        ]
