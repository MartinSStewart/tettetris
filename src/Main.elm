module Main exposing (..)

import Array2 exposing (Array2)
import BlockGroup
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Keyboard
import TypedPoint2 exposing (TypedPoint2(..))
import Time
import Model exposing (..)


---- MODEL ----


init : ( Model, Cmd Msg )
init =
    ( { grid = Array2.init (TypedPoint2 { x = 15, y = 15 }) Empty |> Array2.set TypedPoint2.zero BlockCell
      , fullSize = TypedPoint2 { x = 25, y = 25 }
      , blockGroups = []
      , gridOffset = TypedPoint2 { x = 5, y = 4 }
      }
        |> addBlockGroup (TypedPoint2 { x = 5, y = 8 }) 1
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
                        TypedPoint2 { x = -1, y = 0 }
                    else if keyCode == 38 || keyCode == 87 then
                        TypedPoint2 { x = 0, y = -1 }
                    else if keyCode == 39 || keyCode == 68 then
                        TypedPoint2 { x = 1, y = 0 }
                    else if keyCode == 40 || keyCode == 83 then
                        TypedPoint2 { x = 0, y = 1 }
                    else
                        TypedPoint2.zero

                margin =
                    TypedPoint2 { x = 3, y = 3 }

                newGridOffset =
                    TypedPoint2.add model.gridOffset movement
                        |> TypedPoint2.clamp margin (TypedPoint2.sub model.fullSize margin)
            in
                ( { model | gridOffset = newGridOffset }, Cmd.none )


addBlockGroup : TypedPoint2 WorldPosition Int -> Int -> Model -> Model
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
            blockGroup.blocks
                |> List.map (BlockGroup.blockLocalToWorld blockGroup)
                |> List.foldl
                    (\block grid ->
                        setGridValue model block BlockCell grid
                    )
                    model.grid
    }


collides : Model -> BlockGroup -> Bool
collides model blockGroup =
    blockGroup.blocks
        |> List.map (BlockGroup.blockLocalToWorld blockGroup)
        |> List.any
            (\a ->
                if getGridValue model a == Empty then
                    False
                else
                    True
            )


getGridValue : Model -> TypedPoint2 WorldPosition Int -> GridCell
getGridValue model gridPosition =
    let
        (TypedPoint2 gridLocalCoord) =
            TypedPoint2.sub gridPosition model.gridOffset
    in
        Array2.get (TypedPoint2 gridLocalCoord) model.grid |> Maybe.withDefault Empty


setGridValue :
    Model
    -> TypedPoint2 WorldPosition Int
    -> GridCell
    -> Array2 GridLocalPosition GridCell
    -> Array2 GridLocalPosition GridCell
setGridValue model position gridCell grid =
    let
        (TypedPoint2 gridLocalCoord) =
            TypedPoint2.sub position model.gridOffset
    in
        Array2.set (TypedPoint2 gridLocalCoord) gridCell grid



---- VIEW ----


gridToView :
    Model
    -> TypedPoint2 WorldPosition Int
    -> TypedPoint2 ViewPosition Float
    -> TypedPoint2 ViewPosition Float
gridToView model (TypedPoint2 gridPosition) gridViewSize =
    gridCellSize model.fullSize gridViewSize
        |> TypedPoint2.mult (TypedPoint2.toFloat (TypedPoint2 gridPosition))


view : Model -> Html Msg
view model =
    div []
        [ viewGrid model TypedPoint2.zero (TypedPoint2 { x = 500, y = 500 })
        ]


viewGrid : Model -> TypedPoint2 ViewPosition number -> TypedPoint2 ViewPosition Float -> Html Msg
viewGrid model topLeft size =
    let
        cellSize =
            gridCellSize model.fullSize size

        getViewBlock gridTypedPoint2 =
            viewBlock (gridToView model gridTypedPoint2 size) cellSize

        blockGroupBlocks =
            model.blockGroups
                |> List.concatMap (\a -> a.blocks |> List.map (BlockGroup.blockLocalToWorld a))
                |> List.map getViewBlock

        crosshairWidth =
            2

        (TypedPoint2 crosshairCenter) =
            gridToView model model.gridOffset size

        (TypedPoint2 rawSize) =
            size

        centerPointCrosshair =
            [ absoluteStyle
                (TypedPoint2 { x = crosshairCenter.x - crosshairWidth * 0.5, y = 0 })
                (TypedPoint2 { x = crosshairWidth, y = rawSize.y })
            , absoluteStyle
                (TypedPoint2 { x = 0, y = crosshairCenter.y - crosshairWidth * 0.5 })
                (TypedPoint2 { x = rawSize.x, y = crosshairWidth })
            ]
                |> List.map (\a -> div [ Html.Attributes.style <| ( "background-color", "black" ) :: a ] [])

        blockDivs =
            Array2.toIndexedList model.grid
                |> List.filterMap
                    (\( TypedPoint2 pos, value ) ->
                        case value of
                            Empty ->
                                Nothing

                            BlockCell ->
                                pos
                                    |> TypedPoint2
                                    |> TypedPoint2.add model.gridOffset
                                    |> getViewBlock
                                    |> Just
                    )
    in
        div
            [ Html.Attributes.style <| (absoluteStyle topLeft size) ]
            (blockDivs ++ blockGroupBlocks ++ centerPointCrosshair)


viewBlock : TypedPoint2 ViewPosition number -> TypedPoint2 ViewPosition number2 -> Html Msg
viewBlock topLeft size =
    div
        [ Html.Attributes.style <|
            ( "background-color", "black" )
                :: absoluteStyle topLeft size
        ]
        []


gridCellSize : TypedPoint2 WorldPosition Int -> TypedPoint2 ViewPosition Float -> TypedPoint2 ViewPosition Float
gridCellSize (TypedPoint2 gridDivs) gridViewSize =
    TypedPoint2 gridDivs
        |> TypedPoint2.toFloat
        |> TypedPoint2.inverse
        |> TypedPoint2.mult gridViewSize


absoluteStyle : TypedPoint2 ViewPosition number -> TypedPoint2 ViewPosition number2 -> List ( String, String )
absoluteStyle (TypedPoint2 position) (TypedPoint2 size) =
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
