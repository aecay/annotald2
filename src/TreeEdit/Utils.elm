module TreeEdit.Utils exposing ( fromJust
                               , splice
                               , insert
                               , insertMany
                               , sort2
                               , cmds -- TODO: remove
                               , cmd -- TODO: remove
                               , message
                               , maybeAndThen2 -- TODO: remove?
                               , o
                               , removeAt
                               )

import Debug
import List

import Array.Hamt as Array exposing (Array)
import Cmd.Extra
import Monocle.Lens exposing (Lens)
import Monocle.Optional as Optional exposing (Optional)
import Return exposing (ReturnF)

-- Zero-based indexing, returns items [i,j)
splice : Int -> Int -> Array a -> (Array a, Array a, Array a)
splice i j array =
    let
        pre = Array.slice 0 i array
        span = Array.slice i j array
        post = Array.slice j (Array.length array) array
    in
        (pre, span, post)

insert : Int -> a -> Array a -> Array a
insert i x array =
    insertMany i (Array.repeat 1 x) array

insertMany : Int -> Array a -> Array a -> Array a
insertMany i xs array =
    let
        start = Array.slice 0 i array
        end = Array.slice i (Array.length array) array
    in
        Array.append start (Array.append xs end)

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

message : a -> ReturnF a model
message m = Return.command (Cmd.Extra.perform m)

maybeAndThen2 : (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
maybeAndThen2 fn a b =
    case (a, b) of
        (Just ja, Just jb) -> fn ja jb
        _ -> Nothing

o : Lens a b -> Optional a b
o = Optional.fromLens

removeAt : Int -> Array a -> Array a
removeAt idx a =
    Array.append (Array.slice 0 idx a) (Array.slice (idx+1) (Array.length a) a)
