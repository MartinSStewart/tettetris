port module Main exposing (..)

import Array2 exposing (Array2)
import Grid
import Helpers
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Json
import Json.Decode
import Json.Encode
import Keyboard
import Model exposing (..)
import Point2 exposing (Point2(..))
import Random
import Set
import Time


---- PORTS ----


port portOut : Json.Encode.Value -> Cmd msg


port portIn : (Json.Decode.Value -> msg) -> Sub msg



---- MODEL ----


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { grid = Array2.init (Point2.new 24 24) Empty
      , blocks = []
      , gridOffset = Point2.zero
      , gameStarted = False
      , randomSeed = flags.initialSeed * 100000 |> floor |> Random.initialSeed
      , newBlockCountdown = stepsPerNewBlock
      }
        |> Grid.setCrosshairCenter (Point2.div Grid.worldSize 2)
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | Step Float
    | KeyPress Int


stepsPerNewBlock : Int
stepsPerNewBlock =
    5


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Step _ ->
            let
                countdown =
                    model.newBlockCountdown

                newModel =
                    (if countdown <= 0 then
                        { model | newBlockCountdown = stepsPerNewBlock - 1 }
                            |> Grid.addRandomBlock
                     else
                        { model | newBlockCountdown = countdown - 1 }
                    )
                        |> Grid.step

                filledLines =
                    Grid.filledLines newModel.grid

                removeFilled set get model =
                    Set.foldl
                        (\a b -> { model | grid = model.grid |> set a Empty })
                        model
                        (get filledLines)
            in
                ( newModel
                    |> removeFilled Array2.setRow .rows
                    |> removeFilled Array2.setColumn .columns
                , Cmd.none
                )

        KeyPress keyCode ->
            if model.gameStarted then
                ( gameStartedKeyPress keyCode model, Cmd.none )
            else
                startGame model


startGame : { b | gameStarted : Bool } -> ( { b | gameStarted : Bool }, Cmd msg )
startGame model =
    ( { model | gameStarted = True }
    , PlaySound { soundName = "explosion.mp3", loop = False }
        |> Json.encodePortOutMsg
        |> portOut
    )


gameStartedKeyPress : Keyboard.KeyCode -> Model -> Model
gameStartedKeyPress keyCode model =
    let
        newModel =
            if keyCode == 37 || keyCode == 65 then
                Grid.moveCrosshair 2 model
            else if keyCode == 38 || keyCode == 87 then
                Grid.moveCrosshair 1 model
            else if keyCode == 39 || keyCode == 68 then
                Grid.moveCrosshair 0 model
            else if keyCode == 40 || keyCode == 83 then
                Grid.moveCrosshair 3 model
            else if keyCode == 32 then
                { model | blocks = List.map (Grid.rotateBlock model) model.blocks }
            else
                model

        margin =
            Point2.new Grid.crosshairMargin Grid.crosshairMargin
    in
        if
            Point2.inRectangle
                margin
                (margin |> Point2.sub Grid.worldSize)
                (Grid.getCrosshairCenter newModel)
        then
            newModel
        else
            model


view : Model -> Html Msg
view model =
    let
        viewPortSize =
            Point2.new 800 800

        pressToStart =
            if model.gameStarted then
                div [] []
            else
                div [ Html.Attributes.style <| ( "text-align", "center" ) :: Helpers.absoluteStyle (viewPortSize |> Point2.yOnly |> flip Point2.scale 0.4) viewPortSize ]
                    [ text "Press WASD or Arrow keys to start!" ]
    in
        div []
            [ pressToStart
            , Grid.viewGrid model Point2.zero viewPortSize
            ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.gameStarted then
            Time.every 500 Step
          else
            Sub.none
        , Keyboard.downs KeyPress
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
