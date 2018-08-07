module Json exposing (..)

import Model exposing (..)
import Json.Encode as Enc exposing (object)
import Json.Decode as Dec exposing (andThen, field)
import Point2 exposing (Point2(..))
import Random
import Array2 exposing (Array2)
import Array as Array


-- Json encoders and decoders are created using https://dkodaj.github.io/decgen/


type alias Record_soundName_String_ =
    { soundName : String }


type alias Record_soundName_String_loop_Bool_ =
    { soundName : String, loop : Bool }


decodeBlock : Dec.Decoder Block
decodeBlock =
    Dec.map5
        Block
        (field "blocks" (Dec.list decodePoint2Int))
        (field "rotationHalfOffset" Dec.bool)
        (field "rotation" decodeDirection)
        (field "position" decodePoint2Int)
        (field "direction" decodeDirection)


decodeArray2 : Dec.Decoder (Array2 a GridCell)
decodeArray2 =
    Dec.map2
        Array2
        (field "size" decodePoint2Int)
        (field "data" (Dec.array (Dec.array decodeGridCell)))


encodeArray2 :
    { c | data : Array.Array (Array.Array b), size : Point2 a Int }
    -> Enc.Value
encodeArray2 a =
    object
        [ ( "size", encodePoint2Int a.size )
        , ( "data", (Enc.array << (Array.map (Enc.array << (Array.map encodeGridCell)))) a.data )
        ]


decodePoint2Int : Dec.Decoder (Point2 a Int)
decodePoint2Int =
    Dec.map2 (\a b -> Point2.new a b) Dec.int Dec.int


encodePoint2Int : Point2 a Int -> Enc.Value
encodePoint2Int (Point2 a) =
    object
        [ ( "x", Enc.int a.x )
        , ( "y", Enc.int a.y )
        ]


decodeBlockCoord : Dec.Decoder BlockCoord
decodeBlockCoord =
    Dec.succeed BlockCoord


decodeDirection : Dec.Decoder Int
decodeDirection =
    Dec.int


decodeFlags : Dec.Decoder Flags
decodeFlags =
    Dec.map
        Flags
        (field "initialSeed" Dec.float)


decodeGridCell : Dec.Decoder GridCell
decodeGridCell =
    let
        recover x =
            case x of
                "Empty" ->
                    Dec.succeed Empty

                "BlockCell" ->
                    Dec.succeed BlockCell

                other ->
                    Dec.fail <| "Unknown constructor for type GridCell: " ++ other
    in
        Dec.string |> andThen recover


decodeGridCoord : Dec.Decoder GridCoord
decodeGridCoord =
    Dec.succeed GridCoord


decodeRandomSeed : Dec.Decoder Int
decodeRandomSeed =
    Dec.int


encodeRandomSeed : a -> Int -> Enc.Value
encodeRandomSeed a =
    Enc.int


decodeModel : Dec.Decoder Model
decodeModel =
    Dec.map5
        (\a b c d e -> Model a b c d (Random.initialSeed 0) e)
        (field "grid" decodeArray2)
        (field "blocks" (Dec.list decodeBlock))
        (field "gridOffset" decodePoint2Int)
        (field "gameStarted" Dec.bool)
        (field "newBlockCountdown" Dec.int)


decodePortOutMsg : Dec.Decoder PortOutMsg
decodePortOutMsg =
    Dec.field "Constructor" Dec.string |> andThen decodePortOutMsgHelp


decodePortOutMsgHelp : String -> Dec.Decoder PortOutMsg
decodePortOutMsgHelp constructor =
    case constructor of
        "PlaySound" ->
            Dec.map
                PlaySound
                (field "A1" decodeRecord_soundName_String_loop_Bool_)

        "StopSound" ->
            Dec.map
                StopSound
                (field "A1" decodeRecord_soundName_String_)

        other ->
            Dec.fail <| "Unknown constructor for type PortOutMsg: " ++ other


decodeRecord_soundName_String_ : Dec.Decoder Record_soundName_String_
decodeRecord_soundName_String_ =
    Dec.map
        Record_soundName_String_
        (field "soundName" Dec.string)


decodeRecord_soundName_String_loop_Bool_ : Dec.Decoder Record_soundName_String_loop_Bool_
decodeRecord_soundName_String_loop_Bool_ =
    Dec.map2
        Record_soundName_String_loop_Bool_
        (field "soundName" Dec.string)
        (field "loop" Dec.bool)


decodeViewCoord : Dec.Decoder ViewCoord
decodeViewCoord =
    Dec.succeed ViewCoord


decodeWorldCoord : Dec.Decoder WorldCoord
decodeWorldCoord =
    Dec.succeed WorldCoord


encodeBlock :
    { b
        | blocks : List (Point2 a Int)
        , direction : Int
        , position : Point2 a1 Int
        , rotation : Int
        , rotationHalfOffset : Bool
    }
    -> Enc.Value
encodeBlock a =
    object
        [ ( "blocks", (Enc.list << (List.map encodePoint2Int)) a.blocks )
        , ( "rotationHalfOffset", Enc.bool a.rotationHalfOffset )
        , ( "rotation", encodeDirection a.rotation )
        , ( "position", encodePoint2Int a.position )
        , ( "direction", encodeDirection a.direction )
        ]


encodeBlockCoord : a -> Enc.Value
encodeBlockCoord a =
    Enc.string "BlockCoord"


encodeDirection : Int -> Enc.Value
encodeDirection a =
    Enc.int a


encodeFlags : { a | initialSeed : Float } -> Enc.Value
encodeFlags a =
    object
        [ ( "initialSeed", Enc.float a.initialSeed )
        ]


encodeGridCell : a -> Enc.Value
encodeGridCell a =
    Enc.string <| toString a


encodeGridCoord : a -> Enc.Value
encodeGridCoord a =
    Enc.string "GridCoord"


encodeModel : Model -> Enc.Value
encodeModel a =
    object
        [ ( "grid", encodeArray2 a.grid )
        , ( "blocks", (Enc.list << (List.map encodeBlock)) a.blocks )
        , ( "gridOffset", encodePoint2Int a.gridOffset )
        , ( "gameStarted", Enc.bool a.gameStarted )
        , ( "newBlockCountdown", Enc.int a.newBlockCountdown )
        ]


encodePortOutMsg : PortOutMsg -> Enc.Value
encodePortOutMsg a =
    case a of
        PlaySound a1 ->
            object
                [ ( "Constructor", Enc.string "PlaySound" )
                , ( "A1", encodeRecord_soundName_String_loop_Bool_ a1 )
                ]

        StopSound a1 ->
            object
                [ ( "Constructor", Enc.string "StopSound" )
                , ( "A1", encodeRecord_soundName_String_ a1 )
                ]


encodeRecord_soundName_String_ : { a | soundName : String } -> Enc.Value
encodeRecord_soundName_String_ a =
    object
        [ ( "soundName", Enc.string a.soundName )
        ]


encodeRecord_soundName_String_loop_Bool_ :
    { a | loop : Bool, soundName : String }
    -> Enc.Value
encodeRecord_soundName_String_loop_Bool_ a =
    object
        [ ( "soundName", Enc.string a.soundName )
        , ( "loop", Enc.bool a.loop )
        ]


encodeViewCoord : a -> Enc.Value
encodeViewCoord a =
    Enc.string "ViewCoord"


encodeWorldCoord : a -> Enc.Value
encodeWorldCoord a =
    Enc.string "WorldCoord"
