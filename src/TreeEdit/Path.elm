module TreeEdit.Path exposing ( Path
                              , PathFragment
                              , singleton
                              , childPath
                              , root
                              , parent
                              -- , splitCommon
                              , join
                              , isFragEmpty
                              -- , shiftOne
                              , foot
                              , allCombos
                              -- , toFragment
                              , internals
                              , advance
                              , commonPrefix
                              , intFromFrag
                              , daughterOf
                              , subtract
                              , behead
                              , decompose
                              )

import List.Extra

import TreeEdit.Utils exposing (findSuffix)

-- TODO: import type defs from tree, use Id type
type Path = Path String (List Int)

type PathFragment = Children (List Int)

internals :
    { fragmentChildren : PathFragment -> Maybe (List Int)
    , path : String -> List Int -> Path
    }
internals =
    let
        fragmentChildren f = case f of
                                 Children l -> Just l
    in
        { path = Path
        , fragmentChildren = fragmentChildren
        }

childPath : Int -> Path -> Path
childPath idx path =
    case path of
        Path id children -> Path id <| idx :: children

singleton : String -> Path
singleton id = Path id []

root : Path -> Path
root path =
    case path of
        Path id _ -> Path id []

decompose : Path -> (String, List Int)
decompose (Path id children) = (id, children)

parent : Path -> Maybe Path
parent path =
    case path of
        Path _ [] -> Nothing
        Path id (_ :: rest) -> Just <| Path id rest

foot : Path -> Maybe Int
foot p =
    case p of
        Path _ [] -> Nothing
        Path _ (foot :: _) -> Just foot

join : Path -> PathFragment -> Path
join path fragment =
    case path of
        Path id children ->
            case fragment of
                Children c -> Path id <| c ++ children

intFromFrag : PathFragment -> Maybe Int
intFromFrag frag =
    case frag of
        Children (x :: []) -> Just x
        _ -> Nothing

behead : PathFragment -> Maybe (Int, PathFragment)
behead f =
    case f of
        Children x ->
            let
                head = List.Extra.last x
                path = List.Extra.init x |> Maybe.map Children
            in
                Maybe.map2 (,) head path

isFragEmpty : PathFragment -> Bool
isFragEmpty p =
    case p of
        Children [] -> True
        Children _ -> False

-- shiftOne : Path -> PathFragment -> (Path, PathFragment)
-- shiftOne p (PF pf) =
--     case pf of
--         [] -> (p, PF pf) -- TODO: this is a bogus case, should be a crash
--         otherwise ->
--             case p of
--                 [] -> ( [List.Extra.last pf |> fromJust], PF (List.Extra.init pf |> fromJust) )
--                 l -> ((List.Extra.last pf |> fromJust) :: l,
--                           PF (List.Extra.init pf |> fromJust))

allCombos : Path -> PathFragment -> List Path
allCombos path frag =
    case frag of
        Children c ->
            let
                tails = List.Extra.tails c
            in
                List.map (join path << Children) tails

daughterOf : Path -> Path -> Bool
daughterOf x y =
    case x of
        Path id children -> case y of
                                Path id2 children2 -> (id == id2 &&
                                                           List.Extra.isSuffixOf children children2 &&
                                                           List.length children < List.length children2)

subtract : Path -> Path -> Maybe PathFragment
subtract parent child =
    if not <| daughterOf parent child
    then Nothing
    else
        case parent of
            Path _ children ->
                case child of
                    Path _ children2 ->
                        let
                            len = List.length children
                            len2 = List.length children2
                        in
                            List.take (len2 - len) children2 |> Children |> Just

advance : Path -> Path
advance p =
    case p of
        Path _ [] -> Debug.crash "can't advance a bare ID path"
        Path id (h :: t) -> Path id (h+1 :: t)

commonPrefix : Path -> Path -> Maybe Path
commonPrefix x y =
    case (x, y) of
        (Path id1 children1, Path id2 children2) ->
            if id1 /= id2
            then Nothing
            else
                Just <| Path id1 <| findSuffix children1 children2
