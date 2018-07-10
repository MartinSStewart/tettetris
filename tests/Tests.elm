module Tests exposing (..)

import Test exposing (..)
import Array2
import Point2 exposing (Point2)
import Main
import Block
import Expect


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \_ ->
                Expect.equal 10 (3 + 7)
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        , test "This test should fail" <|
            \_ ->
                Expect.fail "failed as expected!"
        , test "Set grid value" <|
            \_ ->
                let
                    model =
                        Main.Model (Array2.init (Point2 5 5)) (Point2 10 10) [] Point2.zero
                in
                    Main.setGridValue model Point2.zero (Main.BlockCell Block.Block)
                        |> .grid
                        |> Expect.equal model.grid
        ]
