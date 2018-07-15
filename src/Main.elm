port module Main exposing (..)

import Array2 exposing (Array2)
import Block
import Converters
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Json
import Json.Decode
import Json.Encode
import Keyboard
import List.Extra
import Model exposing (..)
import Point2 exposing (Point2(..))
import Random
import Time


---- PORTS ----


port portOut : Json.Encode.Value -> Cmd msg


port portIn : (Json.Decode.Value -> msg) -> Sub msg



---- MODEL ----


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { grid = Array2.init (Point2.new 25 25) Empty
      , blocks = []
      , gridOffset = Point2.zero
      , gameStarted = False
      , randomSeed = flags.initialSeed * 100000 |> floor |> Random.initialSeed
      , newBlockCountdown = stepsPerNewBlock
      }
        |> setCrosshairCenter (Point2.div worldSize 2)
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
                    if countdown <= 0 then
                        { model | newBlockCountdown = stepsPerNewBlock - 1 }
                            |> addRandomBlock
                    else
                        { model | newBlockCountdown = countdown - 1 }
            in
                ( step newModel, Cmd.none )

        KeyPress keyCode ->
            if model.gameStarted then
                ( gameStartedKeyPress keyCode model, Cmd.none )
            else
                startGame model


tupleCombine : (a -> b -> c) -> ( a, b ) -> c
tupleCombine combine ( a, b ) =
    combine a b


startGame : { b | gameStarted : Bool } -> ( { b | gameStarted : Bool }, Cmd msg )
startGame model =
    ( { model | gameStarted = True }
    , PlaySound { soundName = "explosion.mp3", loop = False }
        |> Json.encodePortOutMsg
        |> portOut
    )


crosshairMargin : number
crosshairMargin =
    10


getRandom : Random.Generator a -> Model -> ( a, Model )
getRandom generator model =
    Random.step generator model.randomSeed
        |> Tuple.mapSecond (\a -> { model | randomSeed = a })


gameStartedKeyPress : Keyboard.KeyCode -> Model -> Model
gameStartedKeyPress keyCode model =
    let
        newModel =
            if keyCode == 37 || keyCode == 65 then
                moveCrosshair 2 model
            else if keyCode == 38 || keyCode == 87 then
                moveCrosshair 1 model
            else if keyCode == 39 || keyCode == 68 then
                moveCrosshair 0 model
            else if keyCode == 40 || keyCode == 83 then
                moveCrosshair 3 model
            else if keyCode == 32 then
                { model | blocks = List.map (rotateBlock model) model.blocks }
            else
                model

        margin =
            Point2.new crosshairMargin crosshairMargin
    in
        if
            Point2.inRectangle
                margin
                (margin |> Point2.sub worldSize)
                (getCrosshairCenter newModel)
        then
            newModel
        else
            model


rotateBlock : GridRecord a -> Block -> Block
rotateBlock model block =
    let
        rotatedBlock =
            { block | rotation = block.rotation + 1 }
    in
        if collides model rotatedBlock then
            block
        else
            rotatedBlock


worldSize : Point2 WorldCoord Int
worldSize =
    Point2.new 50 50


addRandomBlock : Model -> Model
addRandomBlock model =
    model
        |> getRandom
            (List.length Block.shapes
                |> Random.int 0
                |> Random.map
                    (flip List.Extra.getAt Block.shapes
                        >> Maybe.withDefault Block.hook
                    )
            )
        |> tupleCombine (\a b -> addBlock a 0 b)


addBlock : ( List (Point2 BlockCoord Int), Bool ) -> Int -> GridRecord a -> GridRecord a
addBlock blockData direction model =
    appendBlock
        { blocks = blockData |> Tuple.first
        , rotationHalfOffset = blockData |> Tuple.second
        , rotation = 0
        , position =
            worldSize
                |> Point2.xOnly
                |> Point2.mirrorX
                |> flip Point2.div 2
                |> Point2.rotateBy90 direction
                |> Point2.map2 (\a b -> a // 2 + b) worldSize
        , direction = direction
        }
        model


appendBlock : Block -> GridRecord a -> GridRecord a
appendBlock block model =
    { model | blocks = block :: model.blocks }


step : Model -> Model
step model =
    model.blocks
        |> List.foldl
            (\blockGroup newModel ->
                let
                    movedBlockGroup =
                        Block.move blockGroup.direction blockGroup
                in
                    if collides newModel movedBlockGroup then
                        addBlockGroupToGrid blockGroup newModel
                    else
                        appendBlock movedBlockGroup newModel
            )
            { model | blocks = [] }


moveCrosshair : Direction -> GridRecord a -> GridRecord a
moveCrosshair direction model =
    let
        updatedBlocks =
            model.blocks
                |> List.foldl
                    (\block newModel ->
                        let
                            movedBlock =
                                Block.move (direction + 2) block
                        in
                            if collides newModel movedBlock then
                                if direction + 2 == block.direction then
                                    addBlockGroupToGrid block newModel
                                else
                                    {- if the block and crosshair aren't moving
                                       in opposite directions then we can just
                                       push the block along with the crosshair
                                    -}
                                    Block.move direction block |> flip appendBlock newModel
                            else
                                appendBlock block newModel
                    )
                    { model | blocks = [] }
    in
        { updatedBlocks
            | gridOffset =
                Point2.new 1 0
                    |> Point2.rotateBy90 direction
                    |> Point2.add model.gridOffset
        }


addBlockGroupToGrid : Block -> GridRecord a -> GridRecord a
addBlockGroupToGrid block model =
    Converters.blockToWorld block
        |> List.foldl
            (\block newModel ->
                setGridValue newModel block BlockCell
            )
            model


collides : GridRecord a -> Block -> Bool
collides model block =
    Converters.blockToWorld block
        |> List.any
            (\a ->
                if getGridValue model a == Empty then
                    let
                        (Point2 aRaw) =
                            getCrosshairCenter model |> Point2.sub a
                    in
                        if block.direction == 0 && aRaw.x >= 0 then
                            True
                        else if block.direction == 1 && aRaw.y < 0 then
                            True
                        else if block.direction == 2 && aRaw.x < 0 then
                            True
                        else if block.direction == 3 && aRaw.y >= 0 then
                            True
                        else
                            False
                    -- let
                    --     oppositeSide =
                    --         Point2.new 1 0 |> Point2.rotateBy90 block.direction
                    -- in
                    --     getCrosshairCenter model
                    --         |> Point2.sub a
                    --         |> Point2.map sign
                    --         |> Point2.mult oppositeSide
                    --         |> (==) oppositeSide
                else
                    True
            )


sign : number -> number
sign value =
    if (value + 0) > 0 then
        1
    else if value < 0 then
        -1
    else
        0


getGridValue : GridRecord a -> Point2 WorldCoord Int -> GridCell
getGridValue model gridPosition =
    Array2.get
        (Converters.worldToGrid model.gridOffset gridPosition)
        model.grid
        |> Maybe.withDefault Empty


setGridValue :
    GridRecord a
    -> Point2 WorldCoord Int
    -> GridCell
    -> GridRecord a
setGridValue model position gridCell =
    { model
        | grid =
            Array2.set
                (Converters.worldToGrid model.gridOffset position)
                gridCell
                model.grid
    }


getCrosshairCenter : GridRecord a -> Point2 WorldCoord Int
getCrosshairCenter model =
    Array2.size model.grid
        |> flip Point2.div 2
        |> Point2.unsafeConvert
        |> Point2.add model.gridOffset


setCrosshairCenter : Point2 WorldCoord Int -> GridRecord a -> GridRecord a
setCrosshairCenter position model =
    { model
        | gridOffset =
            Array2.size model.grid
                |> flip Point2.div 2
                |> Point2.unsafeConvert
                |> Point2.sub position
    }



---- VIEW ----


gridToView :
    Point2 WorldCoord Int
    -> Point2 ViewCoord Float
    -> Point2 ViewCoord Float
gridToView gridPosition gridViewSize =
    gridCellSize worldSize gridViewSize
        |> Point2.mult (gridPosition |> Point2.unsafeConvert |> Point2.map toFloat)


view : Model -> Html Msg
view model =
    let
        viewPortSize =
            Point2.new 800 800

        pressToStart =
            if model.gameStarted then
                div [] []
            else
                div [ Html.Attributes.style <| ( "text-align", "center" ) :: absoluteStyle (viewPortSize |> Point2.yOnly |> flip Point2.scale 0.4) viewPortSize ]
                    [ text "Press WASD or Arrow keys to start!" ]
    in
        div []
            [ pressToStart
            , viewGrid model Point2.zero viewPortSize
            ]


viewGrid : Model -> Point2 ViewCoord number -> Point2 ViewCoord Float -> Html Msg
viewGrid model topLeft size =
    let
        cellSize =
            gridCellSize worldSize size

        getViewBlock gridTypedPoint2 =
            viewBlock (gridToView gridTypedPoint2 size) cellSize

        blockGroupBlocks =
            model.blocks
                |> List.concatMap Converters.blockToWorld
                |> List.map getViewBlock

        crosshairWidth =
            2

        crosshairViewCenter =
            gridToView (getCrosshairCenter model) size

        centerPointCrosshair =
            [ absoluteStyle
                (crosshairViewCenter
                    |> Point2.xOnly
                    |> Point2.add (Point2.new (-crosshairWidth * 0.5) 0)
                )
                (size |> Point2.yOnly |> Point2.add (Point2.new crosshairWidth 0))
            , absoluteStyle
                (crosshairViewCenter
                    |> Point2.yOnly
                    |> Point2.add (Point2.new 0 (-crosshairWidth * 0.5))
                )
                (size |> Point2.xOnly |> Point2.add (Point2.new 0 crosshairWidth))
            ]
                |> List.map (\a -> div [ Html.Attributes.style <| ( "background-color", "black" ) :: a ] [])

        blockDivs =
            Array2.toIndexedList model.grid
                |> List.filterMap
                    (\( pos, value ) ->
                        case value of
                            Empty ->
                                Nothing

                            BlockCell ->
                                Converters.gridToWorld model.gridOffset pos
                                    |> getViewBlock
                                    |> Just
                    )
    in
        div
            [ Html.Attributes.style <| (absoluteStyle topLeft size) ]
            (blockDivs ++ blockGroupBlocks ++ centerPointCrosshair)


viewBlock : Point2 ViewCoord number -> Point2 ViewCoord number2 -> Html Msg
viewBlock topLeft size =
    div
        [ Html.Attributes.style <|
            ( "background-color", "black" )
                :: absoluteStyle topLeft size
        ]
        []


gridCellSize : Point2 WorldCoord Int -> Point2 ViewCoord Float -> Point2 ViewCoord Float
gridCellSize (Point2 gridDivs) gridViewSize =
    Point2 gridDivs
        |> Point2.map toFloat
        |> Point2.inverse
        |> Point2.mult gridViewSize


absoluteStyle : Point2 ViewCoord number -> Point2 ViewCoord number2 -> List ( String, String )
absoluteStyle (Point2 position) (Point2 size) =
    [ ( "position", "absolute" )
    , ( "left", px position.x )
    , ( "top", px position.y )
    , ( "width", px size.x )
    , ( "height", px size.y )
    , ( "margin", "0px" )
    ]


px : number -> String
px value =
    toString value ++ "px"



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.gameStarted then
            Time.every 1000 Step
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
