module Utils exposing ( enumerate
                      , fromJust
                      , remove
                      , splice
                      , insert
                      , sort2
                      )

import List
import List.Extra

import Debug

enumerate : List a -> List (Int, a)
enumerate a = List.Extra.zip (List.range 0 <| List.length a - 1) a

remove : Int -> List a -> List a
remove = List.Extra.removeAt

-- Zero-based indexing, returns items [i,j)
splice : Int -> Int -> List a -> (List a, List a, List a)
splice i j list =
    let
        pre = List.take i list
        span = List.drop i list |> List.take (j - i)
        post = List.drop j list
    in
        (pre, span, post)

insert : Int -> a -> List a -> List a
insert i x list =
    List.take i list ++ x :: List.drop i list

fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

sort2 : comparable -> comparable -> (comparable, comparable)
sort2 a b = if a < b then (a, b) else (b, a)
