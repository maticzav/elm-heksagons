import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

r = 40
margin = 1

hexagons =
   [
      (1,1),
      (2,1),
      (3,1),
      (4,1),
      (5,1)
  ]


-- MOVED represnts which lines are moved higher
-- 0: even lines are higher
-- 1: odd lines are higher
movedLines = 1

isMoved : Int -> Int
isMoved a =
   if rem a 2 == movedLines then
      1
   else
      0

withDefault : a -> Maybe a -> a
withDefault default maybe =
    case maybe of
      Just value -> value
      Nothing -> default

maybeToInt = withDefault 0

minIntInList : List (Int) -> Int
minIntInList list =
   maybeToInt (List.minimum list)

maxIntInList : List (Int) -> Int
maxIntInList list =
   maybeToInt (List.maximum list)

calculateHexagonPosition : Int -> Int -> (Int, Int) -> (Int , Int)
calculateHexagonPosition r m (x,y) =
   let
      pos_x = (toFloat x) * ((3 * r + 2 * m) / 2)
      pos_y = (toFloat y + toFloat (isMoved(x)) * (sqrt 3 * r / 2 + m)
   in
      (pos_x, pos_y)

visa : List (Int, Int) -> (Int -> Int) -> Int
visa coordinates isMoved =
   let
      


calculateHexagonsField : (Int, Int) -> Int -> Int -> Int -> (Int, Int)
calculateHexagonsField (x_dev, y_dev) d m r =
   let
      x = (x_dev + 2) * r + (x_dev - 1) * m - (r / 2 - m) * (1 - rem x_dev 2)
      v = sqrt 3 * r / 2
      y = (n + 2) * v + (n - 1) * m + (v + m) * d
   in
      (x,y)


-- calculateFirstAndLastPointsOfField : ((Int,Int), (Int,Int)) -> ((Int,Int), (Int,Int))
-- calculateFirstAndLastPointsOfField ((xrs, xre) , (yrs, yre)) =
--
--
--
-- calculateHexagonCoordinatesOffset : List (Int, Int) -> Int -> Int -> Int -> (Int, Int)
-- calculateHexagonCoordinatesOffset coordinates =
--    let
--
--    in
--       (offsetX, offsetY)
--
--

niceColor : Color
niceColor =
   let
      r = 100
      g = 200
      b = 255
   in
      rgba r g b 0.3

drawHexagon : Int -> Color -> (Int, Int)
drawHexagon r  filling (x, y) =
   ngon 6 r
    |> filled filling
    |> move (x, y)

drawHexagons : List (Int, Int) -> List Form
drawHexagons hexagons =
  List.map (draw_hexagon r) hexagons

draw : List Form -> Element
draw =
  collage (fst field) (snd field)

main : Element
main = draw (draw_hexagons hexagons)
-- main = draw (draw_hexagons hexagons)
