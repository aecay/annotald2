module TreeEdit.Path exposing
    ( Path(..)
    , Fragment
    , advance
    , allCombos
       -- , toFragment

    , behead
    , childPath
    , commonPrefix
    , daughterOf
    , decompose
    , foot
    , intFromFrag
    , internals
    ,  isFragEmpty
       -- , shiftOne

    , join
    ,  parent
       -- , splitCommon

    , root
    , singleton
    , subtract
    , calculateMovement
    , getId
    , Direction(..)
    )

import List.Extra
import TreeEdit.Utils exposing (findSuffix, fromJust)

-- TODO: import type defs from tree, use Id type


type Path = Path String (List Int)


type alias Fragment = List Int


internals :
    { fragmentChildren : Fragment -> Maybe (List Int)
    , path : String -> List Int -> Path
    , fragment : List Int -> Fragment
    }
internals =
    { path = Path
    , fragmentChildren = Just
    , fragment = identity
    }


childPath : Int -> Path -> Path
childPath idx path =
    case path of
        Path id children ->
            Path id <| idx :: children


singleton : String -> Path
singleton id =
    Path id []


root : Path -> Path
root path =
    case path of
        Path id _ ->
            Path id []

getId : Path -> String -- TODO: proper Id type
getId (Path id _) = id


decompose : Path -> ( String, List Int )
decompose (Path id children) =
    ( id, children )


parent : Path -> Maybe Path
parent path =
    case path of
        Path _ [] ->
            Nothing

        Path id (_ :: rest) ->
            Just <| Path id rest


foot : Path -> Maybe Int
foot p =
    case p of
        Path _ [] ->
            Nothing

        Path _ (ft :: _) ->
            Just ft


join : Path -> Fragment -> Path
join path fragment =
    case path of
        Path id children ->
            Path id <| fragment ++ children


intFromFrag : Fragment -> Maybe Int
intFromFrag frag =
    case frag of
        x :: [] ->
            Just x

        _ ->
            Nothing


behead : Fragment -> Maybe ( Int, Fragment )
behead f =
    let
        head = List.Extra.last f
        path = List.Extra.init f
    in
        Maybe.map2 Tuple.pair head path


isFragEmpty : Fragment -> Bool
isFragEmpty p =
    case p of
        [] ->
            True

        _ ->
            False


allCombos : Path -> Fragment -> List Path
allCombos path frag =
    let
        tails = List.Extra.tails frag
    in
        List.map (join path) tails


daughterOf : Path -> Path -> Bool
daughterOf x y =
    case x of
        Path id children ->
            case y of
                Path id2 children2 ->
                    id == id2
                        && List.Extra.isSuffixOf children children2
                        && List.length children < List.length children2


subtract : Path -> Path -> Maybe Fragment
subtract parnt child =
    if parnt == child
    then Just <| []
    else
        if not <| daughterOf parnt child
        then Nothing
        else
            case parnt of
                Path _ children ->
                    case child of
                        Path _ children2 ->
                            let
                                len = List.length children
                                len2 = List.length children2
                            in
                                List.take (len2 - len) children2 |> Just


advance : Path -> Path
advance p =
    case p of
        Path _ [] ->
            Debug.todo "can't advance a bare ID path"

        Path id (h :: t) ->
            Path id (h + 1 :: t)


commonPrefix : Path -> Path -> Maybe Path
commonPrefix (Path id1 children1) (Path id2 children2) =
    if id1 /= id2 then
        Nothing
    else
        Just <| Path id1 <| findSuffix children1 children2

calculateMovement : Path -> Path ->
                    { siblingFrom : Path
                    , siblingTo : Path
                    , tailFrom : Fragment
                    , tailTo : Fragment
                    , adjacency : Direction
                    }
calculateMovement from to =
    let
        prefix = commonPrefix from to
    in
        case prefix of
            Nothing ->
                let
                    sFrom = root from
                    sTo = root to
                    tailFrom = subtract sFrom from |> fromJust
                    tailTo = subtract sTo to |> fromJust
                in
                    { siblingFrom = sFrom
                    , siblingTo = sTo
                    , tailFrom = tailFrom
                    , tailTo = tailTo
                    , adjacency = checkAdjacent sFrom sTo
                    }
            Just pfx ->
                let
                    (childFrom, tailFrom) = subtract pfx from |> Maybe.andThen behead |> fromJust
                    (childTo, tailTo) = subtract pfx to |> Maybe.andThen behead |> fromJust
                    sFrom = childPath childFrom pfx
                    sTo = childPath childTo pfx
                in
                    { siblingFrom = sFrom
                    , siblingTo = sTo
                    , tailFrom = tailFrom
                    , tailTo = tailTo
                    , adjacency = checkAdjacent sFrom sTo
                    }


type Direction = No | Left | Right

checkAdjacent : Path -> Path -> Direction
checkAdjacent (Path id1 l1) (Path id2 l2) =
    if id1 /= id2 then
        No
    else
        case (l1, l2) of
            (h1 :: t1, h2 :: t2) ->
                if t1 == t2 then
                    if h1 - h2 == -1 then
                        Right
                    else if h1 - h2 == 1 then
                        Left
                    else
                        No
                else No
            _ -> No
