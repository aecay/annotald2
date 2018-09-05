module TreeEdit.Tree exposing ( get
                              , set
                              , highestIndex -- Action.elm: coIndex2, doMove, doMovement
                              , moveTo
                              , internals
                              , insertManyAt -- Action.elm: deleteNode
                              , deleteAt
                              , makeTrace
                              , map
                              , illegalLabelChar
                              , index
                              , metadata
                              , children
                              , either
                              , label
                              , info
                              , hasTerminalLabel
                              , isEmpty
                              , isTerminal
                              , path
                              , forestFromList
                              , allFirst -- Action.elm: doMoveToRoot
                              , allLast  -- ditto
                              )

import Array exposing (Array)
import Char
import Dict
import List
import Maybe

import Monocle.Optional as Optional exposing (Optional)
import Monocle.Common exposing ((=>), maybe, (<|>))
import Monocle.Lens as Lens exposing (Lens)

import TreeEdit.Tree.Type as Type exposing (..)

import TreeEdit.OrderedDict as OD
import TreeEdit.Utils as Utils exposing (o, fromJust, removeAt)

import TreeEdit.Index as Index
import TreeEdit.Path as Path exposing (Path, PathFragment)

import TreeEdit.Result as R exposing (succeed, fail)

type alias Result a = R.Result a

-- TODO: I think we get a crash if you select a node and then right click (as
-- for movement) on a leaf node...test this case...

-- Functions we expose for testing only
internals :
    { allLast : Path -> PathFragment -> Forest -> Bool
    , deleteAt : Path -> Forest -> Forest
    , isLastAt : Forest -> Path -> Bool
    , insertAt : Path -> Tree -> Forest -> Forest
    }
internals = { allLast = allLast
            , isLastAt = isLastAt
            , deleteAt = deleteAt
            , insertAt = insertAt
            }

-- Reexports

index : Lens Tree (Maybe Index.Index)
index = Type.index

children : Lens Tree (Array Tree)
children = Type.children

metadata : Lens Tree Metadata
metadata = Type.metadata

label : Lens Tree Label
label = Type.label

info : Lens Tree TreeInfo
info = Type.info

either : (Terminal -> a) -> (TreeInfo -> Array Tree -> a) -> Tree -> a
either = .either Type.private

hasTerminalLabel : Tree -> Bool
hasTerminalLabel tree =
    let
        nt _ _ = False
        t terminal =
            case terminal of
                Ordinary _ _ -> True
                Trace _ _ -> False
                Comment _ -> False
                EmptyCat Pro _ -> False
                EmptyCat Con _ -> False
                EmptyCat Exp _ -> False
                EmptyCat Star _ -> True
                EmptyCat Zero _ -> True
    in
        either t nt tree

isTerminal : Tree -> Bool
isTerminal = either (always True) (\_ _ -> False)

isEmpty : Tree -> Bool
isEmpty =
    let
        t terminal =
            case terminal of
                Ordinary _ _ -> False
                Trace _ _ -> True
                Comment _ -> True
                EmptyCat _ _ -> True
    in
        either t (\_ _ -> False)

fold : (Tree -> a -> a) -> a -> Tree -> a
fold fn init tree =
    let
        c = children.get tree
        v = fn tree init
    in
        Array.foldl (flip (fold fn)) v c

map : (Tree -> Tree) -> Tree -> Tree
map fn tree =
    let
        newTree = fn tree
        c = children.get newTree
    in
        case Array.length c of
            0 -> newTree
            _ -> children.set (Array.map (\x -> map fn x) c) newTree

innerGet : List Int -> Tree -> Tree
innerGet c tree =
    case c of
        [] -> tree
        foot :: parent -> innerGet parent tree |>
                          children.get |>
                          Array.get foot |>
                          fromJust

get : Path -> Forest -> Tree
get path trees =
    let
        (id, cs) = Path.decompose path
    in
        OD.get id trees |> fromJust |> innerGet cs

innerSet : List Int -> Tree -> Tree -> Tree
innerSet c newChild tree =
    case c of
        [] -> newChild
        foot :: parentPath ->
            innerGet parentPath tree |>
            setChild foot newChild |>
            (\x -> innerSet parentPath x tree)

set : Path -> Tree -> Forest -> Forest
set path newChild trees =
    let
        (id, cs) = Path.decompose path
        nc = innerSet cs newChild <| fromJust <| OD.get id trees
    in
         OD.update id (\_ -> Just nc) trees

path : Path -> Lens Forest Tree
path p = Lens (get p) (set p)

setChild : Int -> Tree -> Tree -> Tree
setChild i new =
    Lens.modify children (Array.set i new)

deleteAt : Path -> Forest -> Forest
deleteAt path_ trees =
    let
        parent_ = Path.parent path_
        idx_ = Path.foot path_
    in
        case (parent_, idx_) of
            (Just parent, Just idx) -> Lens.modify ((path parent) <|> children) (removeAt idx) trees
            _ -> trees

insertAt : Path -> Tree -> Forest -> Forest
insertAt path newChild = insertManyAt path <| Array.repeat 1 newChild

insertManyAt : Path -> Array Tree -> Forest -> Forest
insertManyAt path_ newChildren trees =
    let
        parent_ = Path.parent path_
        idx_ = Path.foot path_
    in
        case (parent_, idx_) of
            (Just parent, Just idx) -> Lens.modify ((path parent) <|> children) (Utils.insertMany idx newChildren) trees
            _ -> trees

highestIndex : Tree -> Int
highestIndex t =
    let
        lens = (o index) => maybe => (Optional.fromLens Index.number) |> .getOption
        f d i = Maybe.withDefault 0 (lens d) |> max i
    in
        fold f 0 t

-- Movement

isFirstAt : Forest -> Path -> Bool
isFirstAt _ p = (Path.foot p) == Just 0

allFirst : Path -> PathFragment -> Forest -> Bool
allFirst path frag trees = List.all (isFirstAt trees) (Path.allCombos path frag)

isLastAt : Forest -> Path -> Bool
isLastAt trees path =
    let
        len = Path.parent path |>
              Maybe.map (flip get trees) |>
              Maybe.map children.get |>
              Maybe.map Array.length
        i = Path.foot path
    in
        i == len

allLast : Path -> PathFragment -> Forest -> Bool
allLast path frag trees =
    List.all (isLastAt trees) (Path.allCombos path frag)

isOnlyChildAt : Forest -> Path -> Bool
isOnlyChildAt t p = isFirstAt t p && isLastAt t p

moveTo : Path -> Path -> Forest -> Result (Forest, Path)
moveTo from to trees =
    if isOnlyChildAt trees from
    then R.fail "Can't move only child"
    else
        if Path.daughterOf from to
        then R.fail "Can't move to own child"
        else
            if Path.daughterOf to from
            then -- Movement to own parent
                let
                    fragment = Path.subtract to from |> fromJust
                in
                    case (allFirst to fragment trees, allLast to fragment trees) of
                        (True, False) ->
                            -- Leftward
                            R.succeed <| (performMove from to trees, to)
                        (False, True) ->
                            -- Rightward, advance the destination path by 1 place
                            let
                                toPath = Path.advance to
                            in
                                R.succeed <| (performMove from toPath trees, toPath)
                        (False, False) -> R.fail "can't move from the middle"
                        otherwise -> R.fail "should never happen"
            else
                let
                    prefix = Path.commonPrefix from to |> fromJust
                    suffixFrom = Path.subtract prefix from |> fromJust
                    suffixTo = Path.subtract prefix to |> fromJust
                in
                    case (Path.behead suffixFrom, Path.behead suffixTo) of
                        (Nothing, _) -> R.fail "mysterious error"
                        (_, Nothing) -> R.fail "mysterious error"
                        (Just (sFrom, tailFrom), Just (sTo, tailTo)) ->
                            case sFrom - sTo of
                                -1 ->
                                    -- Rightward
                                    case (allFirst (Path.childPath sTo prefix) tailTo trees,
                                              allLast (Path.childPath sFrom prefix) tailFrom trees) of
                                        (True, True) ->
                                            let
                                                adjPath1 = case Path.isFragEmpty tailFrom of
                                                               True -> Path.join (Path.childPath sFrom prefix) tailTo |>
                                                                       Debug.log "adjusted"
                                                               False -> Path.join prefix suffixTo
                                                adjPath = adjPath1 |> Path.childPath 0
                                            in
                                                R.succeed <| (performMove from adjPath trees, adjPath)
                                        otherwise -> R.fail "can't move to/from the middle"
                                1 ->
                                    -- Leftward
                                    case (allFirst (Path.childPath sFrom prefix) tailFrom trees,
                                              allLast (Path.childPath sTo prefix) tailTo trees) of
                                        (True, True) ->
                                            let
                                                nKids = get to trees |>
                                                        children.get |>
                                                        Array.length
                                                adjPath = Path.join prefix suffixTo |> Path.childPath nKids
                                            in
                                                R.succeed <| (performMove from adjPath trees, adjPath)
                                        otherwise -> R.fail "can't move to/from the middle"
                                otherwise -> R.fail "can't move from non-adjacent siblings"

performMove : Path -> Path -> Forest -> Forest
performMove from to trees =
    let
        moved = get from trees
    in
        deleteAt from trees |>
        insertAt to moved

-- Other

hasDashTag : String -> Label -> Bool
hasDashTag tag label =
    List.member tag <| String.split "-" label

makeTrace : Tree -> Int -> Tree
makeTrace x i =
    let
        lbl = (.get label) x
        (newLabel, traceType) =
            if String.startsWith "W" lbl
            then (String.dropLeft 1 lbl, private.wh)
            else if hasDashTag "CL" lbl
            then (lbl, private.clitic) -- TODO: drop the CL dashtag
            else (lbl, private.extraposition)
    in
        private.makeTrace traceType newLabel i

legalLabelChar : Char -> Bool
legalLabelChar c =
    Char.isUpper c || Char.isDigit c || c == '-' || c == '.' || c == ',' || c == '+'

illegalLabelChar : Char -> Bool
illegalLabelChar = not << legalLabelChar

forestFromList : List Tree -> Forest
forestFromList trees =
    let
        -- TODO: as alternative to fromJust, can we make our own IDs?  Or do
        -- we really want to crash in this case?
        fn t = (metadata.get t |> Dict.get "ID" |> Utils.fromJust, t)
    in
        OD.fromList <| List.map fn trees
