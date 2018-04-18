module TreeEdit.Utils exposing ( fromJust
                               , splice
                               , insert
                               , insertMany
                               , sort2
                               , cmds
                               , cmd
                               )

import Debug
import List

import Cmd.Extra

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

insertMany : Int -> List a -> List a -> List a
insertMany i xs list =
    List.take i list ++ xs ++ List.drop i list

fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

sort2 : comparable -> comparable -> (comparable, comparable)
sort2 a b = if a < b then (a, b) else (b, a)

cmds : List a -> Cmd a
cmds msgs = List.map Cmd.Extra.perform msgs |> Cmd.batch

cmd : a -> Cmd a
cmd = Cmd.Extra.perform
