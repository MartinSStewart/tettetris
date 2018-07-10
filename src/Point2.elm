module Point2 exposing (..)


type Point2 a number
    = Point2
        { x : number
        , y : number
        }


new : number -> number -> Point2 a number
new x y =
    Point2 { x = x, y = y }


add : Point2 a number -> Point2 a number -> Point2 a number
add (Point2 point0) (Point2 point1) =
    new (point0.x + point1.x) (point0.y + point1.y)


sub : Point2 a number -> Point2 a number -> Point2 a number
sub (Point2 point0) (Point2 point1) =
    new (point0.x - point1.x) (point0.y - point1.y)


rsub : Point2 a number -> Point2 a number -> Point2 a number
rsub (Point2 point1) (Point2 point0) =
    new (point0.x - point1.x) (point0.y - point1.y)


multScalar : Point2 a number -> number -> Point2 a number
multScalar (Point2 point) scalar =
    new (point.x * scalar) (point.y * scalar)


{-| Multiplies one point by a scalar but with the parameter order reversed.
-}
rmultScalar : number -> Point2 a number -> Point2 a number
rmultScalar scalar (Point2 point) =
    new (point.x * scalar) (point.y * scalar)


mult : Point2 a number -> Point2 a number -> Point2 a number
mult (Point2 point0) (Point2 point1) =
    new (point0.x * point1.x) (point0.y * point1.y)


div : Point2 a Int -> Int -> Point2 a Int
div (Point2 point) divisor =
    new (point.x // divisor) (point.y // divisor)


{-| Divides one point by an integer but with the parameter order reversed.
-}
rdiv : Int -> Point2 a Int -> Point2 a Int
rdiv divisor (Point2 point) =
    new (point.x // divisor) (point.y // divisor)


negate : Point2 a number -> Point2 a number
negate (Point2 point) =
    new -point.x -point.y


inverse : Point2 a Float -> Point2 a Float
inverse (Point2 point) =
    new (1 / point.x) (1 / point.y)


zero : Point2 a number
zero =
    new 0 0


one : Point2 a number
one =
    new 1 1


min : Point2 a number -> Point2 a number -> Point2 a number
min (Point2 point0) (Point2 point1) =
    --Due to a compiler bug, we need to add 0. Otherwise we can't use number types in Basics.min.
    new (Basics.min (point0.x + 0) point1.x) (Basics.min point0.y point1.y)


max : Point2 a number -> Point2 a number -> Point2 a number
max (Point2 point0) (Point2 point1) =
    --Due to a compiler bug, we need to add 0. Otherwise we can't use number types in Basics.min.
    new (Basics.max (point0.x + 0) point1.x) (Basics.max point0.y point1.y)


floor : Point2 a Float -> Point2 a Int
floor (Point2 float2) =
    new (Basics.floor float2.x) (Basics.floor float2.y)


toFloat : Point2 a Int -> Point2 a Float
toFloat (Point2 int2) =
    new (Basics.toFloat int2.x) (Basics.toFloat int2.y)


toTuple : Point2 a number -> ( number, number )
toTuple (Point2 point) =
    ( point.x, point.y )


fromTuple : ( number, number ) -> Point2 a number
fromTuple ( x, y ) =
    new x y


transpose : Point2 a number -> Point2 a number
transpose (Point2 point) =
    new point.y point.x


intToInt2 : Int -> Int -> Point2 a Int
intToInt2 width int =
    new (int % width) (int // width)


area : Point2 a number -> number
area (Point2 point) =
    point.x * point.y


length : Point2 a Float -> Float
length (Point2 point) =
    point.x * point.x + point.y * point.y |> sqrt


inRectangle : Point2 a Int -> Point2 a Int -> Point2 a Int -> Bool
inRectangle (Point2 topLeft) (Point2 rectangleSize) (Point2 point) =
    let
        assertSize =
            if rectangleSize.x < 0 || rectangleSize.y < 0 then
                Debug.crash "Negative size not allowed."
            else
                ""

        (Point2 bottomRight) =
            add (Point2 topLeft) (Point2 rectangleSize)
    in
        topLeft.x <= point.x && point.x < bottomRight.x && topLeft.y <= point.y && point.y < bottomRight.y


rotateBy90 : Int -> Point2 a number -> Point2 a number
rotateBy90 rotateBy (Point2 point) =
    if rotateBy % 4 == 1 then
        new point.y -point.x
    else if rotateBy % 4 == 2 then
        new -point.x -point.y
    else if rotateBy % 4 == 3 then
        new -point.y point.x
    else
        Point2 point


{-| Returns the angle of this point in radians.
-}
angle : Point2 a Float -> Float
angle (Point2 point) =
    atan2 point.y point.x


clamp : Point2 a number -> Point2 a number -> Point2 a number -> Point2 a number
clamp (Point2 min) (Point2 max) (Point2 value) =
    new (Basics.clamp min.x max.x value.x) (Basics.clamp min.y max.y value.y)


xOnly : Point2 a number -> Point2 a number
xOnly (Point2 point) =
    new point.x 0


yOnly : Point2 a number -> Point2 a number
yOnly (Point2 point) =
    new point.x 0
