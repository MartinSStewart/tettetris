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


scale : Point2 a number -> number -> Point2 a number
scale (Point2 point) scalar =
    new (point.x * scalar) (point.y * scalar)


mult : Point2 a number -> Point2 a number -> Point2 a number
mult (Point2 point0) (Point2 point1) =
    new (point0.x * point1.x) (point0.y * point1.y)


div : Point2 a Int -> Int -> Point2 a Int
div (Point2 point) divisor =
    new (point.x // divisor) (point.y // divisor)


inverse : Point2 a Float -> Point2 a Float
inverse (Point2 point) =
    new (1 / point.x) (1 / point.y)


zero : Point2 a number
zero =
    new 0 0


one : Point2 a number
one =
    new 1 1


toTuple : Point2 a number -> ( number, number )
toTuple (Point2 point) =
    ( point.x, point.y )


fromTuple : ( number, number ) -> Point2 a number
fromTuple ( x, y ) =
    new x y


toList : Point2 a number -> List number
toList (Point2 point) =
    [ point.x, point.y ]


fromList : List number -> Point2 a number
fromList list =
    case list of
        a :: b :: rest ->
            new a b

        a :: rest ->
            new a 0

        _ ->
            zero


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
inRectangle topLeft bottomRight point =
    map3 clamp topLeft bottomRight point == point


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


xOnly : Point2 a number -> Point2 a number
xOnly (Point2 point) =
    new point.x 0


yOnly : Point2 a number -> Point2 a number
yOnly (Point2 point) =
    new 0 point.y


mirrorX : Point2 a number -> Point2 a number
mirrorX (Point2 point) =
    new -point.x point.y


mirrorY : Point2 a number -> Point2 a number
mirrorY (Point2 point) =
    new point.x -point.y


mirrorXY : Point2 a number -> Point2 a number
mirrorXY (Point2 point) =
    new -point.x -point.y


unsafeConvert : Point2 a number -> Point2 b number
unsafeConvert (Point2 point) =
    new point.x point.y


map : (number -> number2) -> Point2 a number -> Point2 a number2
map componentFunc (Point2 point) =
    new (componentFunc point.x) (componentFunc point.y)


map2 :
    (number -> number2 -> number3)
    -> Point2 a number
    -> Point2 a number2
    -> Point2 a number3
map2 componentFunc (Point2 point0) (Point2 point1) =
    new (componentFunc point0.x point1.x) (componentFunc point0.y point1.y)


map3 :
    (number -> number2 -> number3 -> number4)
    -> Point2 a number
    -> Point2 a number2
    -> Point2 a number3
    -> Point2 a number4
map3 componentFunc (Point2 point0) (Point2 point1) (Point2 point2) =
    new (componentFunc point0.x point1.x point2.x)
        (componentFunc point0.y point1.y point2.y)
