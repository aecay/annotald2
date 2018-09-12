module TreeEdit.Tree exposing
    (  allFirst
       -- Action.elm: doMoveToRoot

    ,  allLast
       -- ditto

    , children
    , deleteAt
    , either
    , forestFromList
    , get
    , hasTerminalLabel
    ,  highestIndex
       -- Action.elm: coIndex2, doMove, doMovement

    , illegalLabelChar
    , index
    , info
    ,  insertManyAt
       -- Action.elm: deleteNode

    , internals
    , isEmpty
    , isTerminal
    , label
    , makeTrace
    , map
    , metadata
    , moveTo
    , path
    , set
    )

import Array exposing (Array)
import Char
import Dict
import List
import List.Extra
import Maybe
import Monocle.Common exposing (maybe)
import Monocle.Lens as Lens exposing (Lens)
import Monocle.Optional as Optional exposing (Optional)
import TreeEdit.Index as Index
import TreeEdit.OrderedDict as OD
import TreeEdit.Path as Path exposing (Path, Fragment)
import TreeEdit.Result as R exposing (fail, succeed)
import TreeEdit.Tree.Type as Type exposing (..)
import TreeEdit.Utils as Utils exposing (fromJust, o, removeAt, and, andO, indexOf)


type alias Result a =
    R.Result a



-- TODO: I think we get a crash if you select a node and then right click (as
-- for movement) on a leaf node...test this case...
-- Functions we expose for testing only


internals :
    { allLast : Path -> Fragment -> Forest -> Bool
    , deleteAt : Path -> Forest -> Forest
    , isLastAt : Forest -> Path -> Bool
    , insertAt : Path -> Tree -> Forest -> Forest
    }
internals =
    { allLast = allLast
    , isLastAt = isLastAt
    , deleteAt = deleteAt
    , insertAt = insertAt
    }



-- Reexports


index : Lens Tree (Maybe Index.Index)
index =
    Type.index


children : Lens Tree (Array Tree)
children =
    Type.children


metadata : Lens Tree Metadata
metadata =
    Type.metadata


label : Lens Tree Label
label =
    Type.label


info : Lens Tree TreeInfo
info =
    Type.info


either : (Terminal -> a) -> (TreeInfo -> Array Tree -> a) -> Tree -> a
either = Type.either


hasTerminalLabel : Tree -> Bool
hasTerminalLabel tree =
    let
        nt _ _ =
            False

        t terminal =
            case terminal of
                Ordinary _ _ ->
                    True

                Trace _ _ ->
                    False

                Comment _ ->
                    False

                EmptyCat Pro _ ->
                    False

                EmptyCat Con _ ->
                    False

                EmptyCat Exp _ ->
                    False

                EmptyCat Star _ ->
                    True

                EmptyCat Zero _ ->
                    True
    in
    either t nt tree


isTerminal : Tree -> Bool
isTerminal =
    either (always True) (\_ _ -> False)


isEmpty : Tree -> Bool
isEmpty =
    let
        t terminal =
            case terminal of
                Ordinary _ _ ->
                    False

                Trace _ _ ->
                    True

                Comment _ ->
                    True

                EmptyCat _ _ ->
                    True
    in
    either t (\_ _ -> False)


fold : (Tree -> a -> a) -> a -> Tree -> a
fold fn init tree =
    let
        c =
            children.get tree

        v =
            fn tree init
    in
    Array.foldl (\b a -> fold fn a b) v c


map : (Tree -> Tree) -> Tree -> Tree
map fn tree =
    let
        newTree =
            fn tree

        c =
            children.get newTree
    in
    case Array.length c of
        0 ->
            newTree

        _ ->
            children.set (Array.map (\x -> map fn x) c) newTree


innerGet : List Int -> Tree -> Tree
innerGet c tree =
    case c of
        [] ->
            tree

        foot :: parent ->
            innerGet parent tree
                |> children.get
                |> Array.get foot
                |> fromJust


get : Path -> Forest -> Tree
get p trees =
    let
        ( id, cs ) =
            Path.decompose p
    in
    OD.get id trees |> fromJust |> innerGet cs


innerSet : List Int -> Tree -> Tree -> Tree
innerSet c newChild tree =
    case c of
        [] ->
            newChild

        foot :: parentPath ->
            innerGet parentPath tree
                |> setChild foot newChild
                |> (\x -> innerSet parentPath x tree)


set : Path -> Tree -> Forest -> Forest
set p newChild trees =
    let
        ( id, cs ) = Path.decompose p
        nc = innerSet cs newChild <| fromJust <| OD.get id trees
    in
    OD.update id (\_ -> Just nc) trees


path : Path -> Lens Forest Tree
path p =
    Lens (get p) (set p)


setChild : Int -> Tree -> Tree -> Tree
setChild i new =
    Lens.modify children (Array.set i new)


deleteAt : Path -> Forest -> Forest
deleteAt path_ trees =
    case path_ of
        Path.Path id (foot :: rest) ->
            let
                parent = (Path.Path id rest)
            in
                Lens.modify (path parent |> and children) (removeAt foot) trees
        Path.Path id [] ->
            OD.remove id trees


insertAt : Path -> Tree -> Forest -> Forest
insertAt p newChild =
    insertManyAt p <| Array.repeat 1 newChild


insertManyAt : Path -> Array Tree -> Forest -> Forest
insertManyAt path_ newChildren trees =
    let
        parent_ =
            Path.parent path_

        idx_ =
            Path.foot path_
    in
    case ( parent_, idx_ ) of
        ( Just parent, Just idx ) ->
            Lens.modify (path parent |> and children) (Utils.insertMany idx newChildren) trees

        _ ->
            trees


highestIndex : Tree -> Int
highestIndex t =
    let
        lens =
            o index |> andO maybe |> andO (Optional.fromLens Index.number) |> .getOption

        f d i =
            Maybe.withDefault 0 (lens d) |> max i
    in
    fold f 0 t



-- Movement


isFirstAt : Forest -> Path -> Bool
isFirstAt _ p =
    Path.foot p == Just 0


allFirst : Path -> Fragment -> Forest -> Bool
allFirst p frag trees =
    Path.allCombos p frag
        |> List.filter (\x -> x /= p)
        |> List.all (isFirstAt trees)


isLastAt : Forest -> Path -> Bool
isLastAt trees p =
    let
        len =
            Path.parent p
                |> Maybe.map (\a -> get a trees)
                |> Maybe.map children.get
                |> Maybe.map Array.length
                |> Maybe.map (\x -> x - 1)

        i =
            Path.foot p
    in
    i == len


allLast : Path -> Fragment -> Forest -> Bool
allLast p frag trees =
    Path.allCombos p frag
        |> List.filter (\x -> x /= p)
        |> List.all (isLastAt trees)


isOnlyChildAt : Forest -> Path -> Bool
isOnlyChildAt t p =
    isFirstAt t p && isLastAt t p


checkRootAdjacency : Forest -> String -> String -> Path.Direction
checkRootAdjacency forest from to =
    let
        ids = OD.keys forest
        idxFrom = indexOf from ids
        idxTo = indexOf to ids
        diff = idxFrom - idxTo
    in
        if diff == -1 then
            Path.Right
        else if diff == 1 then
            Path.Left
        else
            Path.No


moveTo : Path -> Path -> Forest -> Result ( Forest, Path )
moveTo from to trees =
    if isOnlyChildAt trees from then
        R.fail "Can't move only child"
    else if Path.daughterOf from to then
        R.fail "Can't move to own child"
    else if Path.daughterOf to from then
        -- Movement to own parent
        let
            (child, rest) = Path.subtract to from |> Maybe.andThen Path.behead |> fromJust
            childPath = Path.childPath child to
        in
            if allFirst childPath rest trees then
                -- Leftward
                R.succeed <| ( performMove from childPath trees, childPath )
            else if allLast childPath rest trees then
                -- Rightward, advance the destination path by 1 place
                let
                    toPath = Path.advance childPath
                in
                    R.succeed <| ( performMove from toPath trees, toPath )
            else
                R.fail "can't move from the middle"
    else
        let
            { siblingFrom, siblingTo, tailFrom, tailTo, adjacency } = Path.calculateMovement from to
            adj2 = case adjacency of
                       Path.No -> checkRootAdjacency trees (Path.getId from) (Path.getId to)
                       _ -> adjacency
        in
        case adj2 of
            Path.No ->
                R.fail "mysterious error"
            Path.Right ->
                if allFirst siblingTo tailTo trees && allLast siblingFrom tailFrom trees then
                    let
                        adjPath1 =
                            case Path.isFragEmpty tailFrom of
                                True ->
                                    Path.join siblingFrom tailTo
                                False ->
                                    to

                        adjPath = adjPath1 |> Path.childPath 0
                    in
                        R.succeed <| ( performMove from adjPath trees, adjPath )
                else
                    R.fail "can't move to/from the middle"
            Path.Left ->
                if allFirst siblingFrom tailFrom trees && allLast siblingTo tailTo trees then
                    let
                        nKids =
                            get to trees
                              |> children.get
                              |> Array.length

                        adjPath =
                            to |> Path.childPath nKids
                    in
                        R.succeed <| ( performMove from adjPath trees, adjPath )
                else
                    R.fail "can't move to/from the middle"


performMove : Path -> Path -> Forest -> Forest
performMove from to trees =
    let
        moved =
            get from trees
    in
    deleteAt from trees
        |> insertAt to moved



-- Other


hasDashTag : String -> Label -> Bool
hasDashTag tag lbl =
    List.member tag <| String.split "-" lbl


makeTrace : Tree -> Int -> Tree
makeTrace x i =
    let
        lbl =
            .get label x

        ( newLabel, traceType ) =
            if String.startsWith "W" lbl then
                ( String.dropLeft 1 lbl, constants.wh )

            else if hasDashTag "CL" lbl then
                ( lbl, constants.clitic )
                -- TODO: drop the CL dashtag

            else
                ( lbl, constants.extraposition )
    in
    traceType { label = newLabel, index = i, metadata = Dict.empty }


legalLabelChar : Char -> Bool
legalLabelChar c =
    Char.isUpper c || Char.isDigit c || c == '-' || c == '.' || c == ',' || c == '+'


illegalLabelChar : Char -> Bool
illegalLabelChar =
    not << legalLabelChar


forestFromList : List Tree -> Forest
forestFromList trees =
    let
        -- TODO: as alternative to fromJust, can we make our own IDs?  Or do
        -- we really want to crash in this case?
        fn t =
            ( metadata.get t |> Dict.get "ID" |> Utils.fromJust, t )
    in
    OD.fromList <| List.map fn trees
