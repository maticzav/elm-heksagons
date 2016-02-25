import Graphics.Element exposing (show)
import String

fortyTwo : Int
fortyTwo =
   42

names : List String
names =
   ["Alice", "Foo", "a"]

book : { title: String, author: String, pages: Int }
book =
   { title = "foo", author = "bar", pages = 100}

longesNameLength : List String -> Maybe Int
longesNameLength names =
   List.maximum (List.map String.length names)

longesName : List String -> Maybe String
longesName names =
   List.head (List.sortBy String.length names)


flippedComparison a b =
    case compare a b of
      LT -> GT
      EQ -> EQ
      GT -> LT


main =
   show [List.sortWith flippedComparison (String.length names)]
