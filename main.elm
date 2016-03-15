import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

r = 40
margin = 1

hexagons =
   trim_coordinates [(0,1)
   ,(0,2)
   ,(0,3)
   ,(0,4)
   ,(1,1)
   ,(1,3)
   ,(1,4)
   ,(2,1)
   ,(2,2)
   ,(2,4)
   ,(3,2)
   ,(3,4)
  ]


trim_coordinates : List (Int, Int) -> List (Int, Int)
trim_coordinates coordinates =
   let
      x_range = toFloat ((maybeToInt (List.maximum (List.map fst coordinates))) - (maybeToInt (List.minimum (List.map fst coordinates))))
      y_range = toFloat ((maybeToInt (List.maximum (List.map snd coordinates))) - (maybeToInt (List.minimum (List.map snd coordinates))))
      x_error = ceiling (x_range / 2.0) - (maybeToInt (List.maximum (List.map fst coordinates))) - (maybeToInt (List.maximum (List.map fst coordinates)) % 2)
      y_error = ceiling (y_range / 2.0) - (maybeToInt (List.maximum (List.map snd coordinates)))
   in
      List.map (modify_coordinate (x_error , y_error)) coordinates


modify_coordinate : (Int, Int) -> (Int, Int) -> (Int, Int)
modify_coordinate init change =
   let
      (x,y) = init
      (x_chg,y_chg) = change
   in
      (x+x_chg, y+y_chg)

maybeToInt = withDefault 0

withDefault : a -> Maybe a -> a
withDefault default maybe =
    case maybe of
      Just value -> value
      Nothing -> default

field = calculateFieldOfCoordinates hexagons r


calculateFieldOfCoordinates : List (Int, Int) -> Float -> {w: Int, h: Int}
calculateFieldOfCoordinates coordinates r =
   let
      y_range = (maybeToInt (List.maximum (List.map fst coordinates))) - (maybeToInt (List.minimum (List.map fst coordinates)))
      x_range = (maybeToInt (List.maximum (List.map snd coordinates))) - (maybeToInt (List.minimum (List.map snd coordinates)))
   in
      {
         -- w = round ((toFloat y_range*2)  * (r + margin) * cos (pi/6)),
         -- h = round ((toFloat x_range) * (r + margin) * (1 + cos (pi/3)))
         w = 600,
         h = 600
      }


draw_hexagon : Float -> (Int, Int) -> Form
draw_hexagon r (x, y) =
  let
    pos_x = (toFloat x) * (r + margin) * (1 + cos (pi/3))
    pos_y = (toFloat (y*2 + (x % 2))) * (r + margin) * cos (pi/6)  --* (x//abs x))
  in
    ngon 6 r
    |> filled niceColor
    |> move (pos_x, pos_y)

draw_hexagons : List (Int, Int) -> List Form
draw_hexagons hexagons =
  List.map (draw_hexagon r) hexagons

draw : List Form -> Element
draw =
  collage field.w field.h



main : Element
main = draw (draw_hexagons hexagons)
-- main = draw (draw_hexagons hexagons)

niceColor : Color
niceColor =
   let
      r = 100
      g = 200
      b = 255
   in
      rgba r g b 0.3
