import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

r = 40
margin = 1

hexagons =
   trim_coordinates [
         (1,1),
         (2,1),
         (3,1),
         (4,1),
         (5,1)
  ]


withDefault : a -> Maybe a -> a
withDefault default maybe =
    case maybe of
      Just value -> value
      Nothing -> default

calculateFieldOfCoordinates : List (Int, Int) -> Int -> Int -> {w: Int, h: Int}
calculateFieldOfCoordinates coordinates r margin =
   let
      x_range = (maybeToInt (List.maximum (List.map fst coordinates))) - (maybeToInt (List.minimum (List.map fst coordinates)))
      y_range = (maybeToInt (List.maximum (List.map snd coordinates))) - (maybeToInt (List.minimum (List.map snd coordinates)))
      x_deviation = maybeToInt (List.maximum (List.map abs (List.map fst coordinates)))
      y_deviation = maybeToInt (List.maximum (List.map abs (List.map snd coordinates)))
   in
      {
         w = x_deviation * 3*r + margin * x_range,
         h = 200
      }

calculateCoordinatesOffset : List (Int, Int) -> Int -> Int -> {x: Int, y: Int}
calculateCoordinatesOffset coordinates r margin =
   let
      minX = (maybeToInt (List.minimum (List.map fst coordinates)))

draw_hexagon : Float -> (Int, Int) -> (Int, Int)  -> Form
draw_hexagon r (x_starting_point, y_starting_point) (x, y) =
  let
    pos_x = (toFloat x) * (r + margin) * (1 + cos (pi/3)) + x_starting_point
    pos_y = (toFloat (y*2 + (x % 2))) * (r + margin) * cos (pi/6) + y_starting_point
  in
    ngon 6 r
    |> filled niceColor
    |> move (pos_x, pos_y)


niceColor : Color
niceColor =
   let
      r = 100
      g = 200
      b = 255
   in
      rgba r g b 0.3


maybeToInt = withDefault 0
field = calculateFieldOfCoordinates hexagons r margin
hexagons_offset = calculateCoordinatesOffset hexagons


draw_hexagons : List (Int, Int) -> List Form
draw_hexagons hexagons =
  List.map (draw_hexagon r) hexagons

draw : List Form -> Element
draw =
  collage field.w field.h



main : Element
main = draw (draw_hexagons hexagons)
-- main = draw (draw_hexagons hexagons)
