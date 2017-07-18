module TreeEdit.Tree exposing (l, t, trace, Tree, either,
                                   -- TODO: exporting all this internal stuff is not
                                   -- the best...
                                   get, set,
                                   highestIndex
                              --, sameRoot
                              , moveTo
                              , internals
                              , destPath
                              , insertAt
                              , fixPathForMovt -- TODO: marginal on exporting this
                              , do
                              , terminalString
                              , isTerminal
                              , isEmpty
                              , extractAt
                              , index
                              , makeTrace
                              , removeIndex
                              , updateChildren
                              , children
                              , constants
                              , asLabeledBrackets
                              , map
                              , receiveTrees
                              )

import List
import List.Extra exposing (getAt)
import Maybe
import Json.Decode as D exposing (list, string, Decoder, dict, field, lazy)
import Dict exposing (Dict)

import TreeEdit.Utils as Utils

import TreeEdit.Index as Index
import TreeEdit.Path as Path exposing (Path(..), PathFragment)

import Monocle.Optional as Optional exposing (Optional)

import TreeEdit.Res as R exposing (succeed, fail, Result)

-- Functions we expose for testing only
internals :
    { allLast : Path -> PathFragment -> Tree -> Bool
    , canMove : Path -> Path -> Tree -> Bool
    , extractAt : Path -> Tree -> R.Result ( Tree, Tree )
    , fixPathForMovt : Path -> Path -> Path
    , isLastAt : Tree -> Path -> Bool
    }
internals = { allLast = allLast
            , canMove = canMove
            , isLastAt = isLastAt
            , extractAt = extractAt
            , fixPathForMovt = fixPathForMovt
            }

type TraceType = Wh | Extraposition | Clitic

type ECType = Pro | Con | Exp | Star | Zero

type Node = Terminal String (Maybe Index.Index) |
    Trace TraceType Int |
    Comment String |
    EmptyCat ECType (Maybe Index.Index) |
    Nonterminal (List Tree) (Maybe Index.Index)

type alias Label = String

type alias Tree = { contents: Node
                  , label: Label
                  }

-- Convenience functions for generating trees for testing
l : String -> String -> Tree
l label text = { label = label
               , contents = Terminal text Nothing
               }

trace : String -> Int -> Tree
trace label index = { label = label
                    , contents = Trace Wh index
                    }
constants :
    { comment : Tree
    , con : Tree
    , czero : Tree
    , pro : Tree
    }
constants =
    { pro = { label = "NP-SBJ"
            , contents = EmptyCat Pro Nothing
            }
    , con = { label = "NP-SBJ"
            , contents = EmptyCat Con Nothing
            }
    , czero = { label = "C"
              , contents = EmptyCat Zero Nothing
              }
    , comment = { label = "CODE"
                , contents = Comment "XXX"
                }
    }

t : String -> List Tree -> Tree
t label children = { label = label
                   , contents = Nonterminal children Nothing
                   }

either : (Tree -> a) -> (Tree -> a) -> Tree -> a
either nt t tree =
    case tree.contents of
        Nonterminal _ _ -> nt tree
        _ -> t tree

children : Optional Tree (List Tree)
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


get : Path -> Tree -> R.Result Tree
get path tree = case path of
                    Path.RootPath -> succeed tree
                    Path.Path foot _ ->
                        get (Path.parent path) tree |>
                        R.map children.getOption |>
                        R.andThen (R.lift "get") |>
                        R.andThen (getAt foot >> R.lift "get")

set : Path -> Tree -> Tree -> R.Result Tree
set path newChild tree = case path of
                             Path.RootPath -> succeed newChild
                             Path.Path foot _ ->
                                 let
                                     parentPath = Path.parent path
                                 in
                                     get parentPath tree |>
                                     R.andThen (setChild foot newChild) |>
                                     R.andThen (\x -> set parentPath x tree)

do : Path.Path -> (Tree -> Tree) -> Tree -> R.Result Tree
do path f tree =
    let
        orig = get path tree
    in
        orig |> R.map f |> R.andThen (\x -> set path x tree)

-- TODO: rewrite with children lens
setChild : Int -> Tree -> Tree -> R.Result Tree
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

extractAt : Path -> Tree -> R.Result (Tree, Tree)
extractAt path tree =
    case path of
        RootPath -> R.fail "extractAt"
        otherwise ->
            let
                parent = Path.parent path
                idx = Path.foot path
                child = get path tree
            in
                do parent (updateChildren (Utils.remove idx)) tree |>
                R.map2 (,) child

insertAt : Path -> Tree -> Tree -> R.Result Tree
insertAt path newChild =
    let
        parent = Path.parent path
        idx = Path.foot path
    in
        do parent (updateChildren (Utils.insert idx newChild))

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
    Debug.log "tree" (get (Path.parent path) tree) |>
    R.map children.getOption |> R.andThen (R.lift "isLastAt") |>
    R.map List.length |> Debug.log "length" |>
    R.map ((==) (Debug.log "target" (Path.foot path + 1))) |> Debug.log "result" |>
    R.withDefault False |>
    Debug.log ("isLastAt " ++ toString path)

allLast : Path -> PathFragment -> Tree -> Bool
allLast path frag tree =
    List.all (isLastAt tree) (Path.allCombos path frag)

allLastForDest : Path -> PathFragment -> Tree -> Bool
allLastForDest path frag =
    case path of
        Path.RootPath -> \_ -> True -- TODO: is this correct??
        otherwise ->
            let
                -- When speaking of a movement destination, the last element
                -- of frag should actually be pointing one after the end of
                -- the list, so we retract it by one here to simplify the
                -- testing logic in allLast
                newFrag = Path.moveFragLeft frag
            in allLast path newFrag

isOnlyChildAt : Tree -> Path -> Bool
isOnlyChildAt t p = isFirstAt t p && isLastAt t p

destPath : Path -> Path -> Tree -> R.Result Path
destPath src newParent tree =
    let
        (common, src1, dest1) = Path.splitCommon src newParent
    in
        case (Path.isFragEmpty src1, Path.isFragEmpty dest1) of
            -- TODO: duplicates logic in canMove
            -- Refactoring idea: make the tests from canMove into
            -- locally-bound (let) functions.  Then return
            -- pathWeNowCalc |> MyMaybe.guard theTestFn
            -- where MyMaybe.guard : Bool -> Maybe a -> Maybe a
            -- MyMaybe.guard flag val = if flag val else Nothing
            -- Movement to own child disallowed
            (True, False) -> R.fail "destPath"
            -- Movement to own parent
            (False, True) -> case Debug.log "first/last" (isFirstAt tree src,
                                                          isLastAt tree src)
                             of
                                 -- Land to parent's left
                                 (True, False) -> Path.shiftOne newParent src1 |>
                                                  Tuple.first |>
                                                  R.succeed
                                 -- Land to parent's right
                                 (False, True) -> Path.shiftOne newParent src1 |>
                                                  Tuple.first |>
                                                  Path.moveRight |>
                                                  R.succeed
                                 otherwise -> R.fail "destPath"
            otherwise ->
                let
                    rightward = Path.lessThan src newParent
                in
                    Path.join common dest1 |>
                    R.succeed |>
                    \x -> R.map2 Path.childPath
                          ( if rightward
                            then succeed 0
                            else get newParent tree |>
                                R.map children.getOption |> R.andThen (R.lift "destPath") |>
                                R.map List.length
                          ) x

-- It is allowed to move SRC to DEST without affecting the word order in the
-- tree if the following conditions are met: remove the common elements on the
-- path from the root to SRC and the path from root to DEST.  Then, the first
-- elements on the remaining path should be immediately adjacent (in the
-- appropriate direction for the intended movement, i.e. Src >> Dest for
-- rightward movt and vice versa).  And, the remaining elements on each path
-- must be the left/rightmost children (appropriately valued for the path to
-- SRC and DEST and whether the movement is leftward or rightward)
canMove : Path -> Path -> Tree -> Bool
canMove src dest tree =
    let
        (common, src1, dest1) = Debug.log "csd" <| Path.splitCommon src dest
        rightward = Debug.log "rightward" <| Path.lessThan src dest
        testFnSrc = if rightward then allLast else allFirst
        testFnDest = if rightward then allFirst else allLastForDest
    in
        Debug.log (toString (common, src1, dest1)) <|
        case (Path.isFragEmpty src1, Path.isFragEmpty dest1) of
            (False, False) ->
                Debug.log "a" (if rightward
                 then Path.areFragsAdjacent src1 dest1
                 else Path.areFragsAdjacent dest1 src1) &&
                Debug.log "b" (uncurry testFnSrc (Debug.log "s1s" <| Path.shiftOne common src1) tree) &&
                Debug.log "c" (uncurry testFnDest (Path.shiftOne common dest1) tree) &&
                Debug.log "d" (not (isOnlyChildAt tree src))
            (False, True) ->
                -- Movement to parent leftward
                if rightward
                then False
                else Debug.log "b'" (uncurry testFnSrc (Path.shiftOne common src1) tree) &&
                Debug.log "d'" (not (isOnlyChildAt tree src))

            otherwise -> False

fixPathForMovt : Path -> Path -> Path
fixPathForMovt src dest =
    let
        (common, src1, dest1) = Path.splitCommon src dest
        rightward = Path.lessThan src dest
    in
        if rightward && Path.isFragSingleton src1
        then Path.join common <| Path.moveFragLeft dest1
        else dest

moveTo : Path -> Path -> Tree -> R.Result Tree
moveTo source dest tree =
    case canMove source dest tree of
        False -> R.fail "can't move: moveTo"
        True ->
            tree |>
            extractAt source |>
            R.andThen (uncurry (insertAt (Debug.log "fpm" <| fixPathForMovt source dest))) |>
            Debug.log "insert"

-- Other

terminalString : Tree -> String
terminalString tree =
    case tree.contents of
        Terminal x _ -> x
        Trace x _ -> case x of
                       Wh -> "*T*"
                       Extraposition -> "*ICH*"
                       Clitic -> "*CL*"
        Comment _ -> "{COM}"
        EmptyCat x _ -> case x of
                            Pro -> "*pro*"
                            Con -> "*con*"
                            Exp -> "*exp*"
                            Star -> "*"
                            Zero -> "0"
        Nonterminal _ _ -> Debug.crash "Can't get the terminalString of a nonterminal"

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
        }

asLabeledBrackets : Tree -> String
asLabeledBrackets t = asLabeledBrax1 t 0

asLabeledBrax1 : Tree -> Int -> String
asLabeledBrax1 tree indent =
    case tree.contents of
        Nonterminal children index -> Debug.crash "not implemented yet"
        Terminal _ index -> "(" ++ tree.label ++
                               (Maybe.withDefault "" <| Maybe.map Index.string index) ++
                               " " ++ terminalString tree ++
                               ")"
        EmptyCat ec index -> "(" ++ tree.label ++ " " ++ terminalString tree ++
                             (Maybe.withDefault "" <| Maybe.map Index.string index) ++
                             ")"
        Trace typ index -> "(" ++ tree.label ++ " " ++ terminalString tree ++
                           "-" ++ toString index ++
                           ")"
        Comment s -> "(CODE " ++ s ++ ")"

type LeafDecoded = LeafDecoded String String (Dict String String)

decodeLeaf : Decoder Tree
decodeLeaf =
    D.map3 LeafDecoded
        (field "label" string)
        (field "text" string)
        (field "metadata" (dict string)) |>
        D.map mungeLeaf

extractIndex : Dict String String -> (Dict String String, Maybe Index.Index)
extractIndex metadata =
    let
        index = Dict.get "index" metadata
        idxtype = Dict.get "idx-type" metadata
        getInt i = i |>
                   Maybe.withDefault (Debug.crash "should never happen") |>
                   String.toInt |>
                   Result.withDefault (Debug.crash "bad index")
        i = case (index, idxtype) of
                (Just n, Just "gap") -> Just <| Index.gap <| getInt index
                (Just n, Just "regular") -> Just <| Index.normal <| getInt index -- TODO: name mismatch
                (Nothing, Nothing) -> Nothing
                _ -> Debug.crash "bad index"
    in
        ( metadata |> Dict.remove "index" |> Dict.remove "idx-type"
        , i
        )

mungeLeaf : LeafDecoded -> Tree
mungeLeaf l =
    case l of LeafDecoded label text metadata -> -- TODO: destructuring is
                                                 -- ugly, use a type alias/
                                                 -- object instead
        case label of
            "CODE" -> { label = "CODE", contents = Comment text }
            _ ->
                let
                    (_, i) = extractIndex metadata
                    trace typ = i |>
                                Maybe.withDefault (Debug.crash "trace missing index") |>
                                .get Index.number |>
                                Trace typ
                in
                    case text of
                        "*pro*" -> { label = label, contents = EmptyCat Pro i }
                        "*con*" -> { label = label, contents = EmptyCat Con i }
                        "*exp*" -> { label = label, contents = EmptyCat Exp i }
                        "*" ->     { label = label, contents = EmptyCat Star i }
                        -- TODO: causes problems if we have legitimately the
                        -- text "0" in a document
                        "0" ->     { label = label, contents = EmptyCat Zero i }
                        "*T*" ->   { label = label , contents = trace Wh }
                        "*ICH*" -> { label = label, contents = trace Extraposition }
                        "*CL*" ->  { label = label, contents = trace Clitic }
                        _ -> { label = label, contents = Terminal text i }

type NTDecoded = NTDecoded String (List Tree) (Dict String String)

decodeNonterminal : Decoder Tree
decodeNonterminal = D.map3 NTDecoded
                    (field "label" string )
                    (field "children" (list <| lazy <| \_ -> decode))
                    (field "metadata" (dict string)) |>
                    D.map mungeNT

mungeNT : NTDecoded -> Tree
mungeNT n =
    case n of NTDecoded label children metadata ->
        let
            (_, i) = extractIndex metadata
        in
            { label = label, contents = Nonterminal children i }

decode : Decoder Tree
decode =
    D.oneOf [ decodeNonterminal
            , decodeLeaf
            ]

receiveTrees : Decoder (List Tree)
receiveTrees = (list decode)