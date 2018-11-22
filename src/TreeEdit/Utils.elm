module TreeEdit.Utils exposing
    (  cmd
    -- TODO: remove
    ,  cmds
    -- TODO: remove
    , findSuffix
    , fromJust
    , insert
    , insertMany
    , internals
    ,  maybeAndThen2
    -- TODO: remove?
    , message
    , o
    , removeAt
    , sort2
    , splice
    , uncurry3
    , and
    , andO
    , indexOf
    )

import Array exposing (Array)
import Cmd.Extra
import List
import Monocle.Lens as Lens exposing (Lens)
import Monocle.Optional as Optional exposing (Optional)
import Return exposing (ReturnF)


internals =
    { findPrefix = findPrefix }



-- Zero-based indexing, returns items [i,j)


splice : Int -> Int -> Array a -> ( Array a, Array a, Array a )
splice i j array =
    let
        pre =
            Array.slice 0 i array

        span =
            Array.slice i j array

        post =
            Array.slice j (Array.length array) array
    in
    ( pre, span, post )


insert : Int -> a -> Array a -> Array a
insert i x array =
    insertMany i (Array.repeat 1 x) array


insertMany : Int -> Array a -> Array a -> Array a
insertMany i xs array =
    let
        start =
            Array.slice 0 i array

        end =
            Array.slice i (Array.length array) array
    in
    Array.append start (Array.append xs end)


fromJust : Maybe a -> a
fromJust x =
    case x of
        Just y ->
            y

        Nothing ->
            Debug.todo "error: fromJust Nothing"


sort2 : comparable -> comparable -> ( comparable, comparable )
sort2 a b =
    if a < b then
        ( a, b )

    else
        ( b, a )


cmds : List a -> Cmd a
cmds msgs =
    List.map Cmd.Extra.perform msgs |> Cmd.batch


cmd : a -> Cmd a
cmd =
    Cmd.Extra.perform


message : a -> ReturnF a model
message m =
    Return.command (Cmd.Extra.perform m)


maybeAndThen2 : (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
maybeAndThen2 fn a b =
    case ( a, b ) of
        ( Just ja, Just jb ) ->
            fn ja jb

        _ ->
            Nothing


o : Lens a b -> Optional a b
o =
    Optional.fromLens


removeAt : Int -> Array a -> Array a
removeAt idx a =
    Array.append (Array.slice 0 idx a) (Array.slice (idx + 1) (Array.length a) a)


findSuffix : List a -> List a -> List a
findSuffix x y =
    List.reverse <| findPrefix (List.reverse x) (List.reverse y)


findPrefix : List a -> List a -> List a
findPrefix x y =
    let
        go xx yy acc =
            case ( xx, yy ) of
                ( [], _ ) ->
                    List.reverse acc

                ( _, [] ) ->
                    List.reverse acc

                ( xh :: xt, yh :: yt ) ->
                    if xh == yh
                    then go xt yt (xh :: acc)
                    else List.reverse acc
    in
    go x y []


uncurry3 : (a -> b -> c -> d) -> ( a, b, c ) -> d
uncurry3 f ( a, b, c ) =
    f a b c

and : Lens b c -> Lens a b -> Lens a c
and y x = Lens.compose x y

andO : Optional b c -> Optional a b -> Optional a c
andO y x = Optional.compose x y

indexOf : comparable -> Array comparable -> Maybe Int
indexOf val array =
     Array.indexedMap Tuple.pair array
         |> Array.filter (\x -> Tuple.second x == val)
         |> Array.get 0
         |> Maybe.map Tuple.first
