module Main exposing (..)

import Array2 exposing (Array2)
import Block exposing (Block)
import BlockGroup exposing (BlockGroup)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Keyboard
import Point2 exposing (Point2)
import Time


---- MODEL ----


type alias Model =
    { grid : Array2 GridCell
    , fullSize : Point2 Int
    , blockGroups : List BlockGroup
    , gridOffset : Point2 Int
    }


type GridCell
    = Empty
    | BlockCell Block


init : ( Model, Cmd Msg )
init =
    ( { grid = Array2.init { x = 15, y = 15 } Empty |> Array2.set (Point2 0 0) (BlockCell Block.Block)
      , fullSize = { x = 25, y = 25 }
      , blockGroups = []
      , gridOffset = { x = 5, y = 4 }
      }
        |> addBlockGroup { x = 5, y = 8 } 1
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | Step Float
    | KeyPress Int


type Position a b
    = Position (Point2 b)


type BlockGroupPosition
    = BlockGroupPosition


type GridLocalPosition
    = GridLocalPosition


type WorldPosition
    = WorldPosition


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Step _ ->
            ( step model, Cmd.none )

        KeyPress keyCode ->
            let
                movement =
                    if keyCode == 37 || keyCode == 65 then
                        Point2 -1 0
                    else if keyCode == 38 || keyCode == 87 then
                        Point2 0 -1
                    else if keyCode == 39 || keyCode == 68 then
                        Point2 1 0
                    else if keyCode == 40 || keyCode == 83 then
                        Point2 0 1
                    else
                        Point2.zero

                margin =
                    { x = 3, y = 3 }

                newGridOffset =
                    Point2.add model.gridOffset movement |> Point2.clamp margin (Point2.sub model.fullSize margin)
            in
                ( { model | gridOffset = newGridOffset }, Cmd.none )


addBlockGroup : Point2 Int -> Int -> Model -> Model
addBlockGroup position direction model =
    { model
        | blockGroups =
            { blocks = BlockGroup.square
            , rotation = 0
            , position = position
            , direction = direction
            }
                :: model.blockGroups
    }


step : Model -> Model
step model =
    model.blockGroups
        |> List.foldl
            (\blockGroup newModel ->
                let
                    movedBlockGroup =
                        BlockGroup.move blockGroup
                in
                    if collides newModel movedBlockGroup then
                        addBlockGroupToGrid blockGroup newModel
                    else
                        { newModel | blockGroups = movedBlockGroup :: newModel.blockGroups }
            )
            { model | blockGroups = [] }


addBlockGroupToGrid : BlockGroup -> Model -> Model
addBlockGroupToGrid blockGroup model =
    { model
        | grid =
            BlockGroup.getBlocksWorldCoord blockGroup
                |> List.foldl
                    (\block grid ->
                        setGridValue model block (BlockCell Block.Block) grid
                    )
                    model.grid
    }


collides : Model -> BlockGroup -> Bool
collides model blockGroup =
    BlockGroup.getBlocksWorldCoord blockGroup
        |> List.any
            (\a ->
                if getGridValue model a == Empty then
                    False
                else
                    True
            )


getGridValue : Model -> Point2 Int -> GridCell
getGridValue model gridPosition =
    let
        gridLocalCoord =
            Point2.sub gridPosition model.gridOffset
    in
        Array2.get gridLocalCoord model.grid |> Maybe.withDefault Empty


setGridValue : Model -> Point2 Int -> GridCell -> Array2 GridCell -> Array2 GridCell
setGridValue model gridPosition gridCell grid =
    let
        gridLocalCoord =
            Point2.sub gridPosition model.gridOffset
    in
        Array2.set gridLocalCoord gridCell grid



---- VIEW ----


gridToView : Model -> Point2 Int -> Point2 Float -> Point2 Float
gridToView model gridPosition gridViewSize =
    gridCellSize model.fullSize gridViewSize
        |> Point2.mult (Point2.toFloat gridPosition)


view : Model -> Html Msg
view model =
    div []
        [ viewGrid model Point2.zero { x = 500, y = 500 }
        ]


viewGrid : Model -> Point2 number -> Point2 Float -> Html Msg
viewGrid model topLeft size =
    let
        cellSize =
            gridCellSize model.fullSize size

        getViewBlock block gridPosition =
            viewBlock block (gridToView model gridPosition size) cellSize

        blockGroupBlocks =
            model.blockGroups
                |> List.concatMap BlockGroup.getBlocksWorldCoord
                |> List.map (getViewBlock {})

        crosshairWidth =
            2

        crosshairCenter =
            gridToView model model.gridOffset size

        centerPointCrosshair =
            [ absoluteStyle { x = crosshairCenter.x - crosshairWidth * 0.5, y = 0 } { x = crosshairWidth, y = size.y }
            , absoluteStyle { x = 0, y = crosshairCenter.y - crosshairWidth * 0.5 } { x = size.x, y = crosshairWidth }
            ]
                |> List.map (\a -> div [ Html.Attributes.style <| ( "background-color", "black" ) :: a ] [])

        blockDivs =
            Array2.toIndexedList model.grid
                |> List.filterMap
                    (\( pos, value ) ->
                        case value of
                            Empty ->
                                Nothing

                            BlockCell block ->
                                pos |> Point2.add model.gridOffset |> getViewBlock block |> Just
                    )
    in
        div
            [ Html.Attributes.style <| (absoluteStyle topLeft size) ]
            (blockDivs ++ blockGroupBlocks ++ centerPointCrosshair)


viewBlock : Block -> Point2 number -> Point2 number2 -> Html Msg
viewBlock block topLeft size =
    div
        [ Html.Attributes.style <|
            ( "background-color", "black" )
                :: absoluteStyle topLeft size
        ]
        []


gridCellSize : Point2 Int -> Point2 Float -> Point2 Float
gridCellSize gridDivs gridSize =
    gridDivs
        |> Point2.toFloat
        |> Point2.inverse
        |> Point2.mult gridSize


absoluteStyle : Point2 number -> Point2 number2 -> List ( String, String )
absoluteStyle pixelPosition pixelSize =
    [ ( "position", "absolute" )
    , ( "left", px pixelPosition.x )
    , ( "top", px pixelPosition.y )
    , ( "width", px pixelSize.x )
    , ( "height", px pixelSize.y )
    , ( "margin", "0px" )
    ]


px : number -> String
px value =
    toString value ++ "px"



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Step
        , Keyboard.downs KeyPress
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
