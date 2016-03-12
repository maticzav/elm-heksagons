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
main = draw (draw_hexagons hexagons)

niceColor : Color
niceColor =
   let
      r = 100
      g = 200
      b = 255
   in
      rgba r g b 0.6
