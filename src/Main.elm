module Main exposing (..)

import Array2 exposing (Array2)
import Block
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Keyboard
import Point2 exposing (Point2(..))
import Time
import Model exposing (..)


---- MODEL ----


init : ( Model, Cmd Msg )
init =
    ( { grid = Array2.init (Point2.new 15 15) Empty |> Array2.set Point2.zero BlockCell
      , fullSize = Point2.new 25 25
      , blockGroups = []
      , gridOffset = Point2.new 5 4
      }
        |> addBlockGroup (Point2.new 5 8) 1
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | Step Float
    | KeyPress Int


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
                        Point2.new -1 0
                    else if keyCode == 38 || keyCode == 87 then
                        Point2.new 0 -1
                    else if keyCode == 39 || keyCode == 68 then
                        Point2.new 1 0
                    else if keyCode == 40 || keyCode == 83 then
                        Point2.new 0 1
                    else
                        Point2.zero

                margin =
                    Point2.new 3 3

                newGridOffset =
                    Point2.add model.gridOffset movement
                        |> Point2.clamp margin (Point2.sub model.fullSize margin)
            in
                ( { model | gridOffset = newGridOffset }, Cmd.none )


addBlockGroup : Point2 WorldCoord Int -> Int -> Model -> Model
addBlockGroup position direction model =
    { model
        | blockGroups =
            { blocks = Block.square
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
                        Block.move blockGroup
                in
                    if collides newModel movedBlockGroup then
                        addBlockGroupToGrid blockGroup newModel
                    else
                        { newModel | blockGroups = movedBlockGroup :: newModel.blockGroups }
            )
            { model | blockGroups = [] }


addBlockGroupToGrid : Block -> Model -> Model
addBlockGroupToGrid blockGroup model =
    { model
        | grid =
            blockGroup.blocks
                |> List.map (Block.blockLocalToWorld blockGroup)
                |> List.foldl
                    (\block grid ->
                        setGridValue model block BlockCell grid
                    )
                    model.grid
    }


collides : Model -> Block -> Bool
collides model blockGroup =
    blockGroup.blocks
        |> List.map (Block.blockLocalToWorld blockGroup)
        |> List.any
            (\a ->
                if getGridValue model a == Empty then
                    False
                else
                    True
            )


getGridValue : Model -> Point2 WorldCoord Int -> GridCell
getGridValue model gridPosition =
    let
        (Point2 gridLocalCoord) =
            Point2.sub gridPosition model.gridOffset
    in
        Array2.get (Point2 gridLocalCoord) model.grid |> Maybe.withDefault Empty


setGridValue :
    Model
    -> Point2 WorldCoord Int
    -> GridCell
    -> Array2 GridCoord GridCell
    -> Array2 GridCoord GridCell
setGridValue model position gridCell grid =
    let
        (Point2 gridLocalCoord) =
            Point2.sub position model.gridOffset
    in
        Array2.set (Point2 gridLocalCoord) gridCell grid



---- VIEW ----


gridToView :
    Model
    -> Point2 WorldCoord Int
    -> Point2 ViewCoord Float
    -> Point2 ViewCoord Float
gridToView model (Point2 gridPosition) gridViewSize =
    gridCellSize model.fullSize gridViewSize
        |> Point2.mult (Point2.toFloat (Point2 gridPosition))


view : Model -> Html Msg
view model =
    div []
        [ viewGrid model Point2.zero (Point2.new 500 500)
        ]


viewGrid : Model -> Point2 ViewCoord number -> Point2 ViewCoord Float -> Html Msg
viewGrid model topLeft size =
    let
        cellSize =
            gridCellSize model.fullSize size

        getViewBlock gridTypedPoint2 =
            viewBlock (gridToView model gridTypedPoint2 size) cellSize

        blockGroupBlocks =
            model.blockGroups
                |> List.concatMap (\a -> a.blocks |> List.map (Block.blockLocalToWorld a))
                |> List.map getViewBlock

        crosshairWidth =
            2

        (Point2 crosshairCenter) =
            gridToView model model.gridOffset size

        (Point2 rawSize) =
            size

        centerPointCrosshair =
            [ absoluteStyle
                (Point2 { x = crosshairCenter.x - crosshairWidth * 0.5, y = 0 })
                (Point2 { x = crosshairWidth, y = rawSize.y })
            , absoluteStyle
                (Point2 { x = 0, y = crosshairCenter.y - crosshairWidth * 0.5 })
                (Point2 { x = rawSize.x, y = crosshairWidth })
            ]
                |> List.map (\a -> div [ Html.Attributes.style <| ( "background-color", "black" ) :: a ] [])

        blockDivs =
            Array2.toIndexedList model.grid
                |> List.filterMap
                    (\( Point2 pos, value ) ->
                        case value of
                            Empty ->
                                Nothing

                            BlockCell ->
                                pos
                                    |> Point2
                                    |> Point2.add model.gridOffset
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
        |> Point2.toFloat
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
