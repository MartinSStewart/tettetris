module Grid exposing (..)

import Array2 exposing (Array2)
import Block
import Color exposing (Color)
import Color.Convert
import Converters
import Helpers
import Html exposing (Html, div)
import Html.Attributes
import List.Extra as List
import Model exposing (..)
import Point2 exposing (Point2(..))
import Random
import Set exposing (Set)


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
                else
                    True
            )


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


getRandom : Random.Generator a -> Model -> ( a, Model )
getRandom generator model =
    Random.step generator model.randomSeed
        |> Tuple.mapSecond (\a -> { model | randomSeed = a })


getGridValue : GridRecord a -> Point2 WorldCoord Int -> GridCell
getGridValue model gridPosition =
    Array2.get
        (Converters.worldToGrid model gridPosition)
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
                (Converters.worldToGrid model position)
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


filledLines : b -> Array2 a b -> Bands
filledLines emptyCell grid =
    let
        (Point2 size) =
            Array2.size grid

        helper length newPoint xyOnly =
            List.range 0 (length - 1)
                |> List.filter
                    (\x ->
                        List.range -detectorMargin (detectorMargin - 1)
                            |> List.all
                                (newPoint x
                                    >> Point2.add (Point2.div grid.size 2 |> xyOnly)
                                    >> flip Array2.get grid
                                    >> (==) (Just emptyCell)
                                )
                    )
                |> Set.fromList
    in
        { rows = helper size.y (flip Point2.new) Point2.xOnly
        , columns = helper size.x Point2.new Point2.yOnly
        }


crosshairMargin : Int
crosshairMargin =
    10


detectorMargin : number
detectorMargin =
    4


detectorHeight : number
detectorHeight =
    10


addRandomBlock : Model -> Model
addRandomBlock model =
    model
        |> getRandom
            (List.length Block.shapes
                |> Random.int 0
                |> Random.map
                    (flip List.getAt Block.shapes
                        >> Maybe.withDefault Block.hook
                    )
            )
        |> Helpers.tupleCombine (\a b -> addBlock a 0 b)


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
                                if (direction + 2) % 4 == block.direction % 4 then
                                    addBlockGroupToGrid block newModel
                                else
                                    {- if the block and crosshair aren't moving
                                       in opposite directions then we can just
                                       push the block along with the crosshair
                                    -}
                                    Block.move direction block
                                        |> flip appendBlock newModel
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



--- Views ----


gridToView :
    Point2 WorldCoord Int
    -> Point2 ViewCoord Float
    -> Point2 ViewCoord Float
gridToView gridPosition gridViewSize =
    gridCellSize worldSize gridViewSize
        |> Point2.mult (gridPosition |> Point2.unsafeConvert |> Point2.map toFloat)


viewGrid : Model -> Point2 ViewCoord number -> Point2 ViewCoord Float -> Html msg
viewGrid model topLeft size =
    let
        cellSize =
            gridCellSize worldSize size

        (Point2 cellSizeRaw) =
            cellSize

        getViewBlock color gridTypedPoint2 =
            viewBlock color (gridToView gridTypedPoint2 size) cellSize

        blockGroupBlocks =
            model.blocks
                |> List.concatMap Converters.blockToWorld
                |> List.map (getViewBlock Color.blue)

        lineWidth =
            2

        crosshairViewCenter =
            gridToView (getCrosshairCenter model) size

        centerPointCrosshair =
            [ ( (crosshairViewCenter
                    |> Point2.xOnly
                    |> Point2.add (Point2.new (-lineWidth * 0.5) 0)
                )
              , (size |> Point2.yOnly |> Point2.add (Point2.new lineWidth 0))
              , Color.lightGray
              )
            , ( (crosshairViewCenter
                    |> Point2.yOnly
                    |> Point2.add (Point2.new 0 (-lineWidth * 0.5))
                )
              , (size |> Point2.xOnly |> Point2.add (Point2.new 0 lineWidth))
              , Color.lightGray
              )
            , ( (crosshairViewCenter
                    |> Point2.add ((Point2.new -detectorHeight detectorMargin) |> Point2.mult cellSize)
                    |> Point2.add (Point2.new 0 (-lineWidth * 0.5))
                )
              , (Point2.new (detectorHeight * 2 * cellSizeRaw.x) lineWidth)
              , Color.black
              )
            , ( (crosshairViewCenter
                    |> Point2.add ((Point2.new -detectorHeight -detectorMargin) |> Point2.mult cellSize)
                    |> Point2.add (Point2.new 0 (-lineWidth * 0.5))
                )
              , (Point2.new (detectorHeight * 2 * cellSizeRaw.x) lineWidth)
              , Color.black
              )
            , ( (crosshairViewCenter
                    |> Point2.add ((Point2.new -detectorMargin -detectorHeight) |> Point2.mult cellSize)
                    |> Point2.add (Point2.new 0 (-lineWidth * 0.5))
                )
              , (Point2.new lineWidth (detectorHeight * 2 * cellSizeRaw.y))
              , Color.black
              )
            , ( (crosshairViewCenter
                    |> Point2.add ((Point2.new detectorMargin -detectorHeight) |> Point2.mult cellSize)
                    |> Point2.add (Point2.new 0 (-lineWidth * 0.5))
                )
              , (Point2.new lineWidth (detectorHeight * 2 * cellSizeRaw.y))
              , Color.black
              )
            ]
                |> List.map
                    (\( a, b, color ) ->
                        div
                            [ Html.Attributes.style <|
                                ( "background-color", Color.Convert.colorToCssRgba color )
                                    :: Helpers.absoluteStyle a b
                            ]
                            []
                    )

        blockDivs =
            Array2.toIndexedList model.grid
                |> List.filterMap
                    (\( pos, value ) ->
                        case value of
                            Empty ->
                                Nothing

                            BlockCell ->
                                Converters.gridToWorld model.gridOffset pos
                                    |> getViewBlock Color.black
                                    |> Just
                    )
    in
        div
            [ Html.Attributes.style <| (Helpers.absoluteStyle topLeft size) ]
            (centerPointCrosshair ++ blockDivs ++ blockGroupBlocks)


viewBlock : Color -> Point2 ViewCoord number -> Point2 ViewCoord number2 -> Html msg
viewBlock color topLeft size =
    div
        [ Html.Attributes.style <|
            ( "background-color", Color.Convert.colorToCssRgba color )
                :: Helpers.absoluteStyle topLeft size
        ]
        []


gridCellSize : Point2 WorldCoord Int -> Point2 ViewCoord Float -> Point2 ViewCoord Float
gridCellSize (Point2 gridDivs) gridViewSize =
    Point2 gridDivs
        |> Point2.map toFloat
        |> Point2.inverse
        |> Point2.mult gridViewSize


type alias Bands =
    { rows : Set Int, columns : Set Int }


invertBands : Point2 a Int -> Bands -> Bands
invertBands (Point2 gridSize) { rows, columns } =
    { rows = gridSize.y - 1 |> List.range 0 |> Set.fromList |> flip Set.diff rows
    , columns = gridSize.x - 1 |> List.range 0 |> Set.fromList |> flip Set.diff columns
    }


{-| Removes given rows and columns by shifting adjacent rows and columns.
-}
compactify : Bands -> Array2 a GridCell -> Array2 a GridCell
compactify linesToRemove oldGrid =
    let
        notRemoved =
            linesToRemove |> invertBands oldGrid.size

        (Point2 size) =
            oldGrid.size
    in
        oldGrid
            |> compactHelper linesToRemove.columns identity
            |> compactHelper linesToRemove.rows Point2.transpose


compactHelper :
    Set Int
    -> (Point2 a Int -> Point2 a Int)
    -> Array2 a GridCell
    -> Array2 a GridCell
compactHelper removeLines transpose grid =
    let
        (Point2 size) =
            grid.size |> transpose

        offset =
            setCount ((>) (size.x // 2)) removeLines
    in
        Array2.toIndexedList grid
            |> List.filter
                (\( pos, value ) ->
                    pos
                        |> transpose
                        |> (\(Point2 c) -> c.x)
                        |> flip Set.member removeLines
                        |> not
                        |> (&&) (value /= Empty)
                )
            |> List.foldl
                (\( pos, value ) b ->
                    let
                        newPos =
                            removeLines
                                |> setCount (\a -> a < (pos |> transpose |> (\(Point2 c) -> c.x)))
                                |> flip Point2.new 0
                                |> transpose
                                |> Point2.add pos
                    in
                        Array2.set newPos value b
                )
                (Array2.init grid.size Empty)


charSelector : GridCell -> Char
charSelector gridCell =
    case gridCell of
        BlockCell ->
            'x'

        Empty ->
            '.'


setCount : (comparable -> Bool) -> Set comparable -> Int
setCount counter set =
    set |> Set.filter counter |> Set.size
