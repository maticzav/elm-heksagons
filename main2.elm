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
      pos_y = (toFloat y + toFloat (rem x 2)) * (sqrt 3 * r / 2 + m)
   in
      (pos_x, pos_y)

-- hexagonPosition = calculateHexagonPosition r margin

-- calculateHexagonCoordinatesOffset : List (Int, Int) -> (Float, Float)
-- calculateHexagonCoordinatesOffset coordinates =
--    let
--       (minX, minY) = hexagonPosition (minIntInList (List.map fst coordinates), minIntInList (List.map snd coordinates))
--       (maxX, maxY) = hexagonPosition (maxIntInList (List.map fst coordinates), maxIntInList (List.map snd coordinates))
--       offsetX = (toFloat maxX - toFloat minX) / 2.0
--       offsetY = (toFloat maxY - toFloat minY) / 2.0
--    in
--       (offsetX, offsetY)


calculateHexagonsField : List (Int, Int) -> Int -> Int -> (Int, Int)
calculateHexagonsField (x_dev, y_dev) m r =
   let
      x = (x_dev + 2) * r + (x_dev - 1) * m - (r / 2 - m) * (1 - rem x_dev 2)
      v = sqrt 3 * r / 2
      d = length (List.filter (a -> Bool) List a)
      y = (n + 2) * v + (n - 1) * m + (v + m) * d
   in
      (x,y)


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
