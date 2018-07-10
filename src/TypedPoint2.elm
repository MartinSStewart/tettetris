module TypedPoint2 exposing (..)


type TypedPoint2 a number
    = TypedPoint2
        { x : number
        , y : number
        }


add : TypedPoint2 a number -> TypedPoint2 a number -> TypedPoint2 a number
add (TypedPoint2 point0) (TypedPoint2 point1) =
    TypedPoint2 { x = point0.x + point1.x, y = point0.y + point1.y }


sub : TypedPoint2 a number -> TypedPoint2 a number -> TypedPoint2 a number
sub (TypedPoint2 point0) (TypedPoint2 point1) =
    TypedPoint2 { x = point0.x - point1.x, y = point0.y - point1.y }


rsub : TypedPoint2 a number -> TypedPoint2 a number -> TypedPoint2 a number
rsub (TypedPoint2 point1) (TypedPoint2 point0) =
    TypedPoint2 { x = point0.x - point1.x, y = point0.y - point1.y }


multScalar : TypedPoint2 a number -> number -> TypedPoint2 a number
multScalar (TypedPoint2 point) scalar =
    TypedPoint2 { x = point.x * scalar, y = point.y * scalar }


{-| Multiplies one point by a scalar but with the parameter order reversed.
-}
rmultScalar : number -> TypedPoint2 a number -> TypedPoint2 a number
rmultScalar scalar (TypedPoint2 point) =
    TypedPoint2 { x = point.x * scalar, y = point.y * scalar }


mult : TypedPoint2 a number -> TypedPoint2 a number -> TypedPoint2 a number
mult (TypedPoint2 point0) (TypedPoint2 point1) =
    TypedPoint2 { x = point0.x * point1.x, y = point0.y * point1.y }


div : TypedPoint2 a Int -> Int -> TypedPoint2 a Int
div (TypedPoint2 point) divisor =
    TypedPoint2 { x = point.x // divisor, y = point.y // divisor }


{-| Divides one point by an integer but with the parameter order reversed.
-}
rdiv : Int -> TypedPoint2 a Int -> TypedPoint2 a Int
rdiv divisor (TypedPoint2 point) =
    TypedPoint2 { x = point.x // divisor, y = point.y // divisor }


negate : TypedPoint2 a number -> TypedPoint2 a number
negate (TypedPoint2 point) =
    TypedPoint2 { x = -point.x, y = -point.y }


inverse : TypedPoint2 a Float -> TypedPoint2 a Float
inverse (TypedPoint2 point) =
    TypedPoint2 { x = 1 / point.x, y = 1 / point.y }


zero : TypedPoint2 a number
zero =
    TypedPoint2 { x = 0, y = 0 }


one : TypedPoint2 a number
one =
    TypedPoint2 { x = 1, y = 1 }


min : TypedPoint2 a number -> TypedPoint2 a number -> TypedPoint2 a number
min (TypedPoint2 point0) (TypedPoint2 point1) =
    --Due to a compiler bug, we need to add 0. Otherwise we can't use number types in Basics.min.
    TypedPoint2 { x = Basics.min (point0.x + 0) point1.x, y = Basics.min point0.y point1.y }


max : TypedPoint2 a number -> TypedPoint2 a number -> TypedPoint2 a number
max (TypedPoint2 point0) (TypedPoint2 point1) =
    --Due to a compiler bug, we need to add 0. Otherwise we can't use number types in Basics.min.
    TypedPoint2 { x = Basics.max (point0.x + 0) point1.x, y = Basics.max point0.y point1.y }


floor : TypedPoint2 a Float -> TypedPoint2 a Int
floor (TypedPoint2 float2) =
    TypedPoint2 { x = Basics.floor float2.x, y = Basics.floor float2.y }


toFloat : TypedPoint2 a Int -> TypedPoint2 a Float
toFloat (TypedPoint2 int2) =
    TypedPoint2 { x = Basics.toFloat int2.x, y = Basics.toFloat int2.y }


toTuple : TypedPoint2 a number -> ( number, number )
toTuple (TypedPoint2 point) =
    ( point.x, point.y )


fromTuple : ( number, number ) -> TypedPoint2 a number
fromTuple ( x, y ) =
    TypedPoint2 { x = x, y = y }


transpose : { x : a, y : a } -> { x : a, y : a }
transpose point =
    { x = point.y, y = point.x }


intToInt2 : Int -> Int -> TypedPoint2 a Int
intToInt2 width int =
    TypedPoint2 { x = (int % width), y = (int // width) }


area : TypedPoint2 a number -> number
area (TypedPoint2 point) =
    point.x * point.y


length : TypedPoint2 a Float -> Float
length (TypedPoint2 point) =
    point.x * point.x + point.y * point.y |> sqrt


inRectangle : TypedPoint2 a Int -> TypedPoint2 a Int -> TypedPoint2 a Int -> Bool
inRectangle (TypedPoint2 topLeft) (TypedPoint2 rectangleSize) (TypedPoint2 point) =
    let
        assertSize =
            if rectangleSize.x < 0 || rectangleSize.y < 0 then
                Debug.crash "Negative size not allowed."
            else
                ""

        (TypedPoint2 bottomRight) =
            add (TypedPoint2 topLeft) (TypedPoint2 rectangleSize)
    in
        topLeft.x <= point.x && point.x < bottomRight.x && topLeft.y <= point.y && point.y < bottomRight.y


rotateBy90 : Int -> TypedPoint2 a number -> TypedPoint2 a number
rotateBy90 rotateBy (TypedPoint2 point) =
    if rotateBy % 4 == 1 then
        TypedPoint2 { x = point.y, y = -point.x }
    else if rotateBy % 4 == 2 then
        TypedPoint2 { x = -point.x, y = -point.y }
    else if rotateBy % 4 == 3 then
        TypedPoint2 { x = -point.y, y = point.x }
    else
        TypedPoint2 point


{-| Returns the angle of this point in radians.
-}
angle : TypedPoint2 a Float -> Float
angle (TypedPoint2 point) =
    atan2 point.y point.x


clamp : TypedPoint2 a number -> TypedPoint2 a number -> TypedPoint2 a number -> TypedPoint2 a number
clamp (TypedPoint2 min) (TypedPoint2 max) (TypedPoint2 value) =
    TypedPoint2 { x = Basics.clamp min.x max.x value.x, y = Basics.clamp min.y max.y value.y }
