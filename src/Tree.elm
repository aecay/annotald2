module Tree exposing (l, t, trace, Tree, either,
                          -- TODO: exporting all this internal stuff is not
                          -- the best...
                          TreeDatum, get, set,
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
                     )

import List
import List.Extra exposing (getAt)
import Maybe

import Utils

import MultiwayTree as T
import TreeExts as TX
import Index
import Path exposing (Path(..), PathFragment)

import Res as R exposing (succeed, fail, Result)

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

type TraceType = Wh | Extraposition | Clitic | Misc

-- Possible alternative: add (Nonterminal Children) to this enum.  Then
-- type Children = C (List Tree), and rename TreeDatum to tree
type Terminal = Text String | Trace TraceType | Comment String

type alias TreeDatum = { contents: Maybe Terminal
                             -- TODO: we are relying on the code and not the
                             -- typechecker to maintain the invariant that
                             -- only terminal nodes have text
                       , label: String
                       , index: Maybe Index.Index
                       }

type alias Tree = T.Tree TreeDatum

-- Convenience functions for generating trees for testing
l : String -> String -> Tree
l label text = T.Tree { label = label
                      , contents = Just <| Text text
                      , index = Nothing
                      }
               []

trace : String -> Int -> Tree
trace label index = T.Tree { label = label
                           , contents = Just <| Trace Wh
                           , index = Index.normal index |> Just
                      }
               []

t : String -> List Tree -> Tree
t label children = T.Tree { label = label
                          , contents = Nothing
                          , index = Nothing
                          }
                   children

-- TODO: is it ever used?  Should be in TX anyway...
mapChildren : (Tree -> a) -> Tree -> List a
mapChildren f t = List.map f (T.children t)

either : (TreeDatum -> a) -> (TreeDatum -> a) -> Tree -> a
either nt t zipper =
    let
        d = T.datum zipper
        txt = d |> .contents
    in
    case txt of
        Just _ -> t d
        Nothing -> nt d

get : Path -> Tree -> R.Result Tree
get path tree = case path of
                    Path.RootPath -> succeed tree
                    Path.Path foot _ ->
                        get (Path.parent path) tree |>
                        R.map T.children |>
                        R.andThen (getAt foot >> R.lift "get")

set : Path -> Tree -> Tree -> R.Result Tree
set path newChild tree = case path of
                             Path.RootPath -> succeed newChild
                             Path.Path foot _ ->
                                 let
                                     parentPath = Path.parent path
                                 in
                                     get parentPath tree |>
                                     R.andThen (TX.setChild foot newChild) |>
                                     R.andThen (\x -> set parentPath x tree)

do : Path.Path -> (Tree -> Tree) -> Tree -> R.Result Tree
do path f tree =
    let
        orig = get path tree
    in
        orig |> R.map f |> R.andThen (\x -> set path x tree)


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
                do parent (TX.updateChildren (Utils.remove idx)) tree |>
                R.map2 (,) child
                -- get path1 tree ?>
                -- TX.updateChildren (Utils.remove idx) ?>?
                -- (\x -> set path1 x tree) |>
                -- Maybe.map2 (,) child

insertAt : Path -> Tree -> Tree -> R.Result Tree
insertAt path newChild =
    let
        parent = Path.parent path
        idx = Path.foot path
    in
        do parent (TX.updateChildren (Utils.insert idx newChild))

highestIndex : Tree -> Int
highestIndex t =
    let
        fold : TreeDatum -> Int -> Int
        fold d i = Maybe.withDefault 0 (d.index |> Maybe.map .number) |> max i
    in
        T.foldl fold 0 t

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
    R.map T.children |> Debug.log "kids" |>
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
                                R.map T.children |>
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

terminalString : Terminal -> String
terminalString contents =
    case contents of
        Text x -> x
        Trace x -> case x of
                       Wh -> "*T*"
                       Extraposition -> "*ICH*"
                       Clitic -> "*CL*"
                       Misc -> "*"
        Comment _ -> "{COM}"

isTerminal : Tree -> Bool
isTerminal t = case t |> T.datum |> .contents of
                   Nothing -> False
                   Just x -> case x of
                                 Text _ -> False
                                 Trace _ -> True
                                 Comment _ -> True

isEmpty : Tree -> Bool
isEmpty t = case t |> T.datum |> .contents of
                Nothing -> False -- TODO: what notion of emptiness are we
                                 -- working with?  Does it make more sense to
                                 -- say True, or to check whehter all the
                                 -- child nodes are empty?
                Just x -> case x of
                              Text _ -> False
                              Trace _ -> True
                              Comment _ -> True
