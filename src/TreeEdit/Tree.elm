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
                              , updateChildren
                              , children
                              , constants
                              , map
                              , metadata
                              , illegalLabelChar
                              )

import Char
import List
import List.Extra exposing (getAt, removeAt)
import Dict
import Maybe

import Maybe.Extra
import Monocle.Optional as Optional exposing (Optional)
import Monocle.Common exposing ((=>), maybe)
import Monocle.Lens as Lens exposing (Lens)

import TreeEdit.Tree.Type as Type exposing (..)

import TreeEdit.Utils as Utils

import TreeEdit.Index as Index
import TreeEdit.Path as Path exposing (Path(..), PathFragment)

import TreeEdit.Result as R exposing (succeed, fail)

type alias Result a = R.Result a

-- Functions we expose for testing only
internals :
    { allLast : Path -> PathFragment -> Tree -> Bool
    , extractAt : Path -> Tree -> Maybe ( Tree, Tree )
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

children : Lens Tree (List Tree)
children =
    let
        get t =
            case t.contents of
                Nonterminal c _ -> c
                _ -> []
        set c t =
            case t.contents of
                Nonterminal _ index -> { t | contents = Nonterminal c index }
                _ -> t
    in
        Lens get set

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


get : Path -> Tree -> Maybe Tree
get path tree = case path of
                    Path.RootPath -> Just tree
                    Path.Path foot _ ->
                        get (Path.parent path) tree |>
                        Maybe.map children.get |>
                        Maybe.andThen (getAt foot)

set : Path -> Tree -> Tree -> Maybe Tree
set path newChild tree = case path of
                             Path.RootPath -> Just newChild
                             Path.Path foot _ ->
                                 let
                                     parentPath = Path.parent path
                                 in
                                     get parentPath tree |>
                                     Maybe.map (setChild foot newChild) |>
                                     Maybe.andThen (\x -> set parentPath x tree)

do : Path.Path -> (Tree -> Tree) -> Tree -> Maybe Tree
do path f tree =
    let
        orig = get path tree
    in
        orig |> Maybe.map f |> Maybe.andThen (\x -> set path x tree)

setChild : Int -> Tree -> Tree -> Tree
setChild i new =
    Lens.modify children (List.Extra.setAt i new)

-- TODO: rewrite with children lens.  It it even needed?
updateChildren : (List Tree -> List Tree) -> Tree -> Tree
updateChildren f t =
    case t.contents of
        Nonterminal children index -> { t | contents = Nonterminal (f children) index }
        _ -> t -- TODO: fail somehow?

extractAt : Path -> Tree -> Maybe (Tree, Tree)
extractAt path tree =
    case path of
        RootPath -> Nothing
        otherwise ->
            let
                parent = Path.parent path
                idx = Path.foot path
                child = get path tree
                newparent = do parent (updateChildren (removeAt idx)) tree
            in
                Maybe.map (,) child |> Maybe.Extra.andMap newparent

insertAt : Path -> Tree -> Tree -> Maybe Tree
insertAt path newChild = insertManyAt path [newChild]

insertManyAt : Path -> List Tree -> Tree -> Maybe Tree
insertManyAt path newChildren =
    let
        parent = Path.parent path
        idx = Path.foot path
    in
        do parent (updateChildren (Utils.insertMany idx newChildren))

-- TODO: the appropriate type here is Optional
index : Lens Tree (Maybe Index.Index)
index =
    let
        get t = case t.contents of
                    Terminal _ idx -> idx
                    Nonterminal _ idx -> idx
                    Trace _ idx -> Just <| Index.normal idx
                    Comment _ -> Nothing
                    EmptyCat _ idx -> idx
        set idx t = case t.contents of
                        Terminal s _ -> { t | contents = Terminal s idx }
                        Nonterminal c _ -> { t | contents = Nonterminal c idx }
                        Trace typ _ ->
                            let
                                i = Maybe.map (.get Index.number) idx
                            in
                                case i of
                                    Just n -> { t | contents = Trace typ n }
                                    Nothing -> Debug.log "Tried to unset index of trace" t
                        EmptyCat typ _ -> { t | contents = EmptyCat typ idx }
                        Comment _ -> Debug.log "Tried to manipulate index of comment" t
    in
        Lens get set

highestIndex : Tree -> Int
highestIndex t =
    let
        lens = (Optional.fromLens index) => maybe => (Optional.fromLens Index.number) |> .getOption
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
    Maybe.map children.get |>
    Maybe.map List.length |>
    Maybe.map ((==) (Path.foot path + 1)) |>
    Maybe.withDefault False

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
                            R.liftVal "moveTo lift" <| performMove from (Path.childPath sFrom common) tree
                        (False, True) ->
                            R.liftVal "moveTo lift" <| performMove from (Path.childPath (sFrom + 1) common) tree
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
                                        R.liftVal "moveTo 2" <| performMove from adjPath tree
                                otherwise -> R.fail "can't move to/from the middle"
                        1 ->
                            -- Leftward
                            case (allFirst (Path.childPath sFrom common) tailFrom tree,
                                      allLast (Path.childPath sTo common) tailTo tree) of
                                (True, True) ->
                                    let
                                        nKids = get to tree |>
                                                Maybe.map children.get |>
                                                Maybe.map List.length |>
                                                R.liftVal "nKids"
                                        adjPath1 = Path.join common fragTo
                                        adjPath = nKids |> R.map (\x -> Path.childPath x adjPath1)
                                    in
                                        adjPath |> R.andThen (\x -> R.liftVal "moveTo 3" <| performMove from x tree)
                                otherwise -> R.fail "can't move to/from the middle"

                        otherwise -> R.fail "can't move from non-adjacent siblings"

performMove : Path -> Path -> Tree -> Maybe (Tree, Path)
performMove from to tree =
    extractAt from tree |>
    Maybe.andThen (uncurry (insertAt to)) |>
    Maybe.map (\x -> (x, to))

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

makeTrace : Tree -> Index.Index -> Tree
makeTrace x i =
    let
        label = x.label
        (newLabel, traceType) =
            if String.startsWith "W" label
            then (String.dropLeft 1 label, Wh)
            else if hasDashTag "CL" label
            then (label, Clitic) -- TODO: drop the CL dashtag
            else (label, Extraposition)
    in
        { contents = Trace traceType i.number
        , label = newLabel
        , metadata = Dict.empty
        }

legalLabelChar : Char -> Bool
legalLabelChar c =
    Char.isUpper c || Char.isDigit c || c == '-' || c == '.' || c == ',' || c == '+'

illegalLabelChar : Char -> Bool
illegalLabelChar = not << legalLabelChar
