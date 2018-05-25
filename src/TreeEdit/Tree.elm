module TreeEdit.Tree exposing ( get
                              , set
                              , highestIndex -- Action.elm: coIndex2, doMove, doMovement
                              , moveTo
                              , internals
                              , insertManyAt -- Action.elm: deleteNode
                              , extractAt
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
                              )

import Char
import List
import List.Extra exposing (getAt, removeAt)
import Maybe

import Monocle.Optional as Optional exposing (Optional)
import Monocle.Common exposing ((=>), maybe, (<|>))
import Monocle.Lens as Lens exposing (Lens)

import TreeEdit.Tree.Type as Type exposing (..)

import TreeEdit.Utils as Utils exposing (o, fromJust)

import TreeEdit.Index as Index
import TreeEdit.Path as Path exposing (Path(..), PathFragment)

import TreeEdit.Result as R exposing (succeed, fail)

type alias Result a = R.Result a

-- Functions we expose for testing only
internals :
    { allLast : Path -> PathFragment -> Tree -> Bool
    , extractAt : Path -> Tree -> ( Tree, Tree )
    , isLastAt : Tree -> Path -> Bool
    }
internals = { allLast = allLast
            , isLastAt = isLastAt
            , extractAt = extractAt
            }

-- Reexports

index : Lens Tree (Maybe Index.Index)
index = Type.index

children : Lens Tree (List Tree)
children = Type.children

metadata : Lens Tree Metadata
metadata = Type.metadata

label : Lens Tree Label
label = Type.label

info : Lens Tree TreeInfo
info = Type.info

either : (Terminal -> a) -> (TreeInfo -> List Tree -> a) -> Tree -> a
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
    in
        case c of
            [] -> fn tree init
            ch ->
                let
                    v = fn tree init
                in
                    List.foldl (flip (fold fn)) v ch

map : (Tree -> Tree) -> Tree -> Tree
map fn tree =
    let
        newTree = fn tree
        c = children.get newTree
    in
        case c of
            [] -> newTree
            ch -> children.set (List.map (\x -> map fn x) ch) newTree


get : Path -> Tree -> Tree
get path tree = case path of
                    Path.RootPath -> tree
                    Path.Path foot _ ->
                        get (Path.parent path) tree |>
                        children.get |>
                        getAt foot |>
                        fromJust

set : Path -> Tree -> Tree -> Tree
set path newChild tree = case path of
                             Path.RootPath -> newChild
                             Path.Path foot _ ->
                                 let
                                     parentPath = Path.parent path
                                 in
                                     get parentPath tree |>
                                     setChild foot newChild |>
                                     (\x -> set parentPath x tree)

path : Path -> Lens Tree Tree
path p = Lens (get p) (set p)

setChild : Int -> Tree -> Tree -> Tree
setChild i new =
    Lens.modify children (List.Extra.setAt i new)

extractAt : Path -> Tree -> (Tree, Tree)
extractAt path_ tree =
    let
        parent = Path.parent path_
        idx = Path.foot path_
        child = get path_ tree
        newparent = Lens.modify ((path parent) <|> children) (removeAt idx) tree
    in
        (child, newparent)

insertAt : Path -> Tree -> Tree -> Tree
insertAt path newChild = insertManyAt path [newChild]

insertManyAt : Path -> List Tree -> Tree -> Tree
insertManyAt path_ newChildren =
    let
        parent = Path.parent path_
        idx = Path.foot path_
    in
        Lens.modify ((path parent) <|> children) (Utils.insertMany idx newChildren)

highestIndex : Tree -> Int
highestIndex t =
    let
        lens = (o index) => maybe => (Optional.fromLens Index.number) |> .getOption
        f d i = Maybe.withDefault 0 (lens d) |> max i
    in
        fold f 0 t

-- Movement

isFirstAt : Tree -> Path -> Bool
isFirstAt _ p =
    case p of
        Path.RootPath -> False -- TODO: true?
        Path.Path foot _ -> foot == 0

allFirst : Path -> PathFragment -> Tree -> Bool
allFirst path frag tree =
    List.all (isFirstAt tree) (Path.allCombos path frag)

isLastAt : Tree -> Path -> Bool
isLastAt tree path =
    get (Path.parent path) tree |>
    children.get |>
    List.length |>
    ((==) (Path.foot path + 1))

allLast : Path -> PathFragment -> Tree -> Bool
allLast path frag tree =
    List.all (isLastAt tree) (Path.allCombos path frag)

isOnlyChildAt : Tree -> Path -> Bool
isOnlyChildAt t p = isFirstAt t p && isLastAt t p

moveTo : Path -> Path -> Tree -> Result (Tree, Path)
moveTo from to tree =
    if isOnlyChildAt tree from
    then R.fail "Can't move only child"
    else
        let
            _ = Debug.log "from" from
            _ = Debug.log "to" to
            { common, sibFrom, sibTo, tailFrom, tailTo, fragFrom, fragTo } = Path.splitCommon from to |> Debug.log "components"
        in
            case (sibFrom, sibTo) of
                (Nothing, _) -> R.fail "Can't move to own child"
                (Just sFrom, Nothing) ->
                    -- Movement to own parent
                    case (allFirst (Path.childPath sFrom common) tailFrom tree,
                              allLast (Path.childPath sFrom common) tailFrom tree) of
                        (True, False) ->
                            -- Leftward
                            R.succeed <| performMove from (Path.childPath sFrom common) tree
                        (False, True) ->
                            R.succeed <| performMove from (Path.childPath (sFrom + 1) common) tree
                        (False, False) -> R.fail "can't move from the middle"
                        otherwise -> R.fail "should never happen"
                (Just sFrom, Just sTo) ->
                    case sFrom - sTo of
                        -1 ->
                            -- Rightward
                            case (allFirst (Path.childPath sTo common) tailTo tree,
                                      allLast (Path.childPath sFrom common) tailFrom tree) of
                                (True, True) ->
                                    let
                                        adjPath1 = case Path.isFragEmpty tailFrom of
                                                       True -> Path.join (Path.childPath sFrom common) tailTo |> Debug.log "adjusted"
                                                       False -> Path.join common fragTo
                                        adjPath = adjPath1 |> Path.childPath 0
                                    in
                                        R.succeed <| performMove from adjPath tree
                                otherwise -> R.fail "can't move to/from the middle"
                        1 ->
                            -- Leftward
                            case (allFirst (Path.childPath sFrom common) tailFrom tree,
                                      allLast (Path.childPath sTo common) tailTo tree) of
                                (True, True) ->
                                    let
                                        nKids = get to tree |>
                                                children.get |>
                                                List.length |>
                                                R.succeed
                                        adjPath1 = Path.join common fragTo
                                        adjPath = nKids |> R.map (\x -> Path.childPath x adjPath1)
                                    in
                                        adjPath |> R.andThen (\x -> R.succeed <| performMove from x tree)
                                otherwise -> R.fail "can't move to/from the middle"

                        otherwise -> R.fail "can't move from non-adjacent siblings"

performMove : Path -> Path -> Tree -> (Tree, Path)
performMove from to tree =
    extractAt from tree |>
    uncurry (insertAt to) |>
    (\x -> (x, to))

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
