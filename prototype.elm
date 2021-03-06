import Graphics.Element exposing (show)

r = 40
margin = 1


hexagons =
   -- [(-1,-1),(-1,0),(-1,1),(-1,2),(0,-1),(0,0),(0,1),(0,2),(1,-1),(1,0),(1,1),(1,2),(2,-1),(2,0),(2,1),(2,2)]
   -- trim_coordinates [(0,1)
   -- ,(0,2)
   -- ,(0,3)
   -- ,(0,4)
   -- ,(1,1)
   -- ,(1,2)
   -- ,(1,3)
   -- ,(1,4)
   -- ,(2,1)
   -- ,(2,2)
   -- ,(2,3)
   -- ,(2,4)
   -- ,(3,1)
   -- ,(3,2)
   -- ,(3,3)
   -- ,(3,4)]

   trim_coordinates [
      (1,1)
  ]



  -- , (2, 0)
  -- , (2, -1)
  -- , (3, 0)
  -- , (3, 1)
  -- , (2, -3)
  -- , (3, 4)
  -- , (-2, 1)
  -- , (-5,4)
  -- , (-2, -3)
  -- , (-3, 4)
  -- , (-1, 1)
  -- , (-2,4)

  -- ]

trim_coordinates : List (Int, Int) -> {x: Int, y: Int}
trim_coordinates coordinates =
   let
      x_range = toFloat ((maybeToInt (List.maximum (List.map fst coordinates))) - () + 1)
      y_range = toFloat ((maybeToInt (List.maximum (List.map snd coordinates))) - (maybeToInt (List.minimum (List.map snd coordinates))))
      x_error = ceiling (x_range / 2.0) - (maybeToInt (List.maximum (List.map fst coordinates))) - (maybeToInt (List.maximum (List.map fst coordinates)) % 2)
      y_error = ceiling (y_range / 2.0) - (maybeToInt (List.maximum (List.map snd coordinates)))
   in
      {
         x: x_error
         y: y_error
      }


minInt : List (Int) -> Int
minInt list =
   maybeToInt (List.minimum list)

maxInt : List (Int) -> Int
maxInt list =
   maybeToInt (List.maximum list)

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


calculateFieldOfCoordinates : List (Int, Int) -> Int -> {w: Int, h: Int}
calculateFieldOfCoordinates coordinates r =
   let
      x_range = (maybeToInt (List.maximum (List.map fst coordinates))) - (maybeToInt (List.minimum (List.map fst coordinates)))
      y_range = (maybeToInt (List.maximum (List.map snd coordinates))) - (maybeToInt (List.minimum (List.map snd coordinates)))
      x_deviation = maybeToInt (List.maximum (List.map abs (List.map fst coordinates)))
      y_deviation = maybeToInt (List.maximum (List.map abs (List.map snd coordinates)))
   in
      {
         w = x_deviation * 3*r + margin * (x_range),
         h = y_deviation * 3*r + margin * (y_range)
      }

field = calculateFieldOfCoordinates hexagons r

main = show [field.w, field.h]
-- main = show hexagons
