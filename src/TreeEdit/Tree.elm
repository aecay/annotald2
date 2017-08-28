module TreeEdit.Tree exposing (l, t, trace, either,
                                   -- TODO: exporting all this internal stuff is not
                                   -- the best...
                                   get, set,
                                   highestIndex
                              --, sameRoot
                              , moveTo
                              , internals
                              , insertAt
                              , insertManyAt
                              , do
                              , isTerminal
                              , isEmpty
                              , extractAt
                              , index
                              , makeTrace
                              , removeIndex
                              , updateChildren
                              , children
                              , constants
                              , map
                              , receiveTrees
                              , metadata
                              , illegalLabelChar
                              )

import Char
import List
import List.Extra exposing (getAt, removeAt)
import Dict
import Maybe
import Json.Decode exposing (Decoder)

import TreeEdit.Tree.Type as Type exposing (..)
import TreeEdit.Tree.Decoder

import TreeEdit.Utils as Utils

import TreeEdit.Index as Index
import TreeEdit.Path as Path exposing (Path(..), PathFragment)

import Monocle.Optional as Optional exposing (Optional)
import Monocle.Lens as Lens exposing (Lens)

import TreeEdit.Result as R exposing (succeed, fail)

type alias Result a = R.Result a

receiveTrees : Decoder (List Tree)
receiveTrees = TreeEdit.Tree.Decoder.receiveTrees

-- Functions we expose for testing only
internals :
    { allLast : Path -> PathFragment -> Tree -> Bool
    , extractAt : Path -> Tree -> Result ( Tree, Tree )
    , isLastAt : Tree -> Path -> Bool
    }
internals = { allLast = allLast
            , isLastAt = isLastAt
            , extractAt = extractAt
            }

-- Convenience functions for generating trees for testing TODO: export as internals
l : String -> String -> Tree
l label text = { label = label
               , contents = Terminal text Nothing
               , metadata = Dict.empty
               }

trace : String -> Int -> Tree
trace label index = { label = label
                    , contents = Trace Wh index
                    , metadata = Dict.empty
                    }
constants :
    { comment : Tree
    , con : Tree
    , czero : Tree
    , pro : Tree
    , vb : Tree
    }
constants =
    { pro = { label = "NP-SBJ"
            , contents = EmptyCat Pro Nothing
            , metadata = Dict.empty
            }
    , con = { label = "NP-SBJ"
            , contents = EmptyCat Con Nothing
            , metadata = Dict.empty
            }
    , czero = { label = "C"
              , contents = EmptyCat Zero Nothing
              , metadata = Dict.empty
              }
    , comment = { label = "CODE"
                , contents = Comment "XXX"
                , metadata = Dict.empty
                }
    , vb = { label = "VB"
           , contents = EmptyCat Star Nothing
           , metadata = Dict.empty
           }
    }

t : String -> List Tree -> Tree
t label children = { label = label
                   , contents = Nonterminal children Nothing
                   , metadata = Dict.empty
                   }

either : (Tree -> a) -> (Tree -> a) -> Tree -> a
either nt t tree =
    case tree.contents of
        Nonterminal _ _ -> nt tree
        _ -> t tree

children : Optional Tree (List Tree) -- TODO: lens, children of terminal is []
children =
    let
        getChildren t =
            case t.contents of
                Nonterminal c _ -> Just c
                _ -> Nothing
        setChildren newChildren tree =
            case tree.contents of
                Nonterminal _ index -> { tree | contents = Nonterminal newChildren index }
                _ -> tree
    in
        Optional getChildren setChildren

fold : (Tree -> a -> a) -> a -> Tree -> a
fold fn init tree =
    let
        c = children.getOption tree
    in
        case c of
            Nothing -> fn tree init
            Just ch ->
                let
                    v = fn tree init
                in
                    List.foldl (flip (fold fn)) v ch

map : (Tree -> Tree) -> Tree -> Tree
map fn tree =
    let
        newTree = fn tree
        c = children.getOption newTree
    in
        case c of
            Nothing -> newTree
            Just ch -> children.set (List.map (\x -> map fn x) ch) newTree


get : Path -> Tree -> Result Tree
get path tree = case path of
                    Path.RootPath -> succeed tree
                    Path.Path foot _ ->
                        get (Path.parent path) tree |>
                        R.andThen (R.lift "no children" children.getOption) |>
                        R.andThen (R.lift "chld not found" (getAt foot))

set : Path -> Tree -> Tree -> Result Tree
set path newChild tree = case path of
                             Path.RootPath -> succeed newChild
                             Path.Path foot _ ->
                                 let
                                     parentPath = Path.parent path
                                 in
                                     get parentPath tree |>
                                     R.andThen (setChild foot newChild) |>
                                     R.andThen (\x -> set parentPath x tree)

do : Path.Path -> (Tree -> Tree) -> Tree -> Result Tree
do path f tree =
    let
        orig = get path tree
    in
        orig |> R.map f |> R.andThen (\x -> set path x tree)

-- TODO: rewrite with children lens
setChild : Int -> Tree -> Tree -> Result Tree
setChild i new parent =
    case parent.contents of
        Nonterminal children index ->
            if List.length children > i
            then R.succeed <| (\x -> { parent | contents = x }) <|
                flip Nonterminal index <|
                List.take i children ++
                [new] ++
                List.drop (i + 1) children
            else R.fail "setChild"
        _ -> R.fail "setChild"

-- TODO: rewrite with children lens.  It it even needed?
updateChildren : (List Tree -> List Tree) -> Tree -> Tree
updateChildren f t =
    case t.contents of
        Nonterminal children index -> { t | contents = Nonterminal (f children) index }
        _ -> t -- TODO: fail somehow?

extractAt : Path -> Tree -> Result (Tree, Tree)
extractAt path tree =
    case path of
        RootPath -> R.fail "extractAt"
        otherwise ->
            let
                parent = Path.parent path
                idx = Path.foot path
                child = get path tree
                newparent = do parent (updateChildren (removeAt idx)) tree
            in
                R.map (,) child |> R.andMap newparent

insertAt : Path -> Tree -> Tree -> Result Tree
insertAt path newChild = insertManyAt path [newChild]

insertManyAt : Path -> List Tree -> Tree -> Result Tree
insertManyAt path newChildren =
    let
        parent = Path.parent path
        idx = Path.foot path
    in
        do parent (updateChildren (Utils.insertMany idx newChildren))

getIndex : Tree -> Maybe Index.Index
getIndex t =
    case t.contents of
        Terminal _ idx -> idx
        Nonterminal _ idx -> idx
        Trace _ idx -> Just <| Index.normal idx
        _ -> Nothing

setIndex : Index.Index -> Tree -> Tree
setIndex idx tree =
    case tree.contents of
        Terminal s _ -> { tree | contents = Terminal s <| Just idx }
        Nonterminal c _ -> { tree | contents = Nonterminal c <| Just idx }
        Trace t _ -> {tree | contents = Trace t <| (.get Index.number) idx }
        _ -> tree

-- TODO: rewrite with let
index : Optional Tree Index.Index
index = Optional getIndex setIndex

-- TODO: rewrite with lens
removeIndex : Tree -> Tree
removeIndex tree =
    case tree.contents of
        Terminal s _ -> { tree | contents = Terminal s Nothing }
        Nonterminal c _ -> { tree | contents = Nonterminal c Nothing }
        _ -> tree -- TODO: fail noisily

highestIndex : Tree -> Int
highestIndex t =
    let
        lens = Optional.composeLens index Index.number |> .getOption
        f d i = Maybe.withDefault 0 (lens d) |> max i
    in
        fold f 0 t

-- Metadata

metadata : Lens Tree Metadata
metadata =
    let
        get = .metadata
        set m t = { t | metadata = m }
    in
        Lens get set

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
    R.andThen (R.lift "no children" children.getOption) |>
    R.map List.length |>
    R.map ((==) (Path.foot path + 1)) |>
    R.withDefault False

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
                            performMove from (Path.childPath sFrom common) tree
                        (False, True) ->
                            performMove from (Path.childPath (sFrom + 1) common) tree
                        (False, False) -> R.fail "can't move from the middle" |> Debug.log "res"
                        otherwise -> R.fail "should never happen" |> Debug.log "res"
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
                                        performMove from adjPath tree
                                otherwise -> R.fail "can't move to/from the middle"
                        1 ->
                            -- Leftward
                            case (allFirst (Path.childPath sFrom common) tailFrom tree,
                                      allLast (Path.childPath sTo common) tailTo tree) of
                                (True, True) ->
                                    let
                                        nKids = get to tree |> R.map (.getOption children >> Utils.fromJust >> List.length)
                                        adjPath1 = Path.join common fragTo
                                        adjPath = nKids |> R.map (\x -> Path.childPath x adjPath1)
                                    in
                                        adjPath |> R.andThen (\x -> performMove from x tree)
                                otherwise -> R.fail "can't move to/from the middle"

                        otherwise -> R.fail "can't move from non-adjacent siblings"

performMove : Path -> Path -> Tree -> Result (Tree, Path)
performMove from to tree =
    let
        _ = Debug.log "from pm" from
        _ = Debug.log "to pm" to
    in
        extractAt from tree |>
        R.andThen (uncurry (insertAt to)) |>
        R.map (\x -> (x, to))

-- Other

isTerminal : Tree -> Bool
isTerminal t = case t.contents of
                   Nonterminal _ _ -> False
                   _ -> True

isEmpty : Tree -> Bool
isEmpty t = case t.contents of
                Terminal _ _ -> False
                Trace _ _ -> True
                Comment _ -> True
                EmptyCat _ _ -> True
                Nonterminal _ _ -> False

hasDashTag : String -> Label -> Bool
hasDashTag tag label =
    List.member tag <| String.split "-" label

makeTrace : Tree -> Tree
makeTrace x =
    let
        label = x.label
        (newLabel, traceType) =
            if String.startsWith "W" label
            then (String.dropLeft 1 label, Wh)
            else if hasDashTag "CL" label
            then (label, Clitic) -- TODO: drop the CL dashtag
            else (label, Extraposition)
    in
        { contents = Trace traceType 0 -- TODO: properly get an index
        , label = newLabel
        , metadata = Dict.empty
        }

legalLabelChar : Char -> Bool
legalLabelChar c =
    Char.isUpper c || Char.isDigit c || c == '=' || c == '-' || c == '.' || c == ','

illegalLabelChar : Char -> Bool
illegalLabelChar = not << legalLabelChar
