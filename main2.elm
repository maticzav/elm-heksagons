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
calculateHexagonPosition r margin coordinates =
   let
      pos_x = (toFloat x) * (r + margin) * (1 + cos (pi/3))
      pos_y = (toFloat (y*2 + (x % 2))) * (r + margin) * cos (pi/6)
   in
      (pos_x, pos_y)

hexagonPosition = calculateHexagonPosition r margin

calculateHexagonCoordinatesOffset : List (Int, Int) -> (Float, Float)
calculateHexagonCoordinatesOffset coordinates =
   let
      (minX, minY) = hexagonPosition (minIntInList (List.map fst coordinates), minIntInList (List.map snd coordinates))
      (maxX, maxY) = hexagonPosition (maxIntInList (List.map fst coordinates), maxIntInList (List.map snd coordinates))
      offsetX = (toFloat maxX - toFloat minX) / 2.0
      offsetY = (toFloat maxY - toFloat minY) / 2.0
   in
      (offsetX, offsetY)
calculateHexagonsField : List (Int, Int) -> (Int, Int)
calculateHexagonsField coordinates =
   let
      (minX, minY) = hexagonPosition (minIntInList (List.map fst coordinates), minIntInList (List.map snd coordinates))
      (maxX, maxY) = hexagonPosition (maxIntInList (List.map fst coordinates), maxIntInList (List.map snd coordinates))
      (w, h) = hexagonPosition ((maxX - minX + 2), (maxY - minY))
   in
      (w, h)


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
