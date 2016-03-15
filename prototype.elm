import Graphics.Element exposing (show)

hexagons =
   -- [(-1,-1),(-1,0),(-1,1),(-1,2),(0,-1),(0,0),(0,1),(0,2),(1,-1),(1,0),(1,1),(1,2),(2,-1),(2,0),(2,1),(2,2)]
   [(0,1)
   ,(0,2)
   ,(0,3)
   ,(0,4)
   ,(1,1)
   ,(1,2)
   ,(1,3)
   ,(1,4)
   ,(2,1)
   ,(2,2)
   ,(2,3)
   ,(2,4)
   ,(3,1)
   ,(3,2)
   ,(3,3)
   ,(3,4)]




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

trim_coordinates : List (Int, Int) -> List (Int, Int)
trim_coordinates coordinates =
   let
      x_range = toFloat ((maybeToInt (List.maximum (List.map fst coordinates))) - (maybeToInt (List.minimum (List.map fst coordinates))))
      y_range = toFloat ((maybeToInt (List.maximum (List.map snd coordinates))) - (maybeToInt (List.minimum (List.map snd coordinates))))
      x_error = ceiling (x_range / 2.0) - 2 * (maybeToInt (List.maximum (List.map fst coordinates)))
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

main = show [hexagons, (trim_coordinates hexagons)]
