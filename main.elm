import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

r = 40
margin = 1

hexagons =
  [ (0, 0)
  , (1, 0)
  , (1, 1)
  , (2, 0)
  , (2, -1)
  , (3, 0)
  , (3, 1)
  , (2, -3)
  , (3, 4)
  , (-2, 1)
  , (-5,4)
  , (-2, -3)
  , (-3, 4)
  , (-1, 1)
  , (-2,4)

  ]

trim_coordinates : List (Int, Int) -> List (Int, Int)
trim_coordinates coordinates =
   let
      x_range = toFloat ((maybeToInt (List.maximum (List.map fst coordinates))) - (maybeToInt (List.minimum (List.map fst coordinates))))
      y_range = toFloat ((maybeToInt (List.maximum (List.map snd coordinates))) - (maybeToInt (List.minimum (List.map snd coordinates))))
      x_error = abs (maybeToInt (List.minimum (List.map fst coordinates))) - ceiling (x_range / 2.0)
      y_error = abs (maybeToInt (List.minimum (List.map snd coordinates))) - ceiling (y_range / 2.0)
   in
      List.map (modify_coordinate (x_error, 0)) coordinates

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

draw_hexagon : Float -> (Int, Int) -> Form
draw_hexagon r (x, y) =
  let
    pos_x = (toFloat x) * (r + margin) * (1 + cos (pi/3))
    pos_y = (toFloat (y - (x % 2) * (x//abs x) + (y % 2) * (y//abs y))) * (r + margin) * cos (pi/6)
  in
    ngon 6 r
    |> filled niceColor
    |> move (pos_x, pos_y)

draw_hexagons : List (Int, Int) -> List Form
draw_hexagons hexagons =
  List.map (draw_hexagon r) hexagons

draw : List Form -> Element
draw =
  collage 600 600

main : Element
-- main = draw (draw_hexagons (trim_coordinates hexagons))
main = draw (draw_hexagons hexagons)

niceColor : Color
niceColor =
   let
      r = 100
      g = 200
      b = 255
   in
      rgba r g b 0.3
