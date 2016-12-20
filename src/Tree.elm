module Tree exposing (l, t, Tree, either,
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
                     )

import List
import List.Extra exposing ((!!))
import Maybe

import Utils exposing ((?>?), (?>))

import MultiwayTree as T
import TreeExts as TX
import Index
import Path exposing (Path(..), PathFragment)

-- Functions we expose for testing only
internals :
    { allLast : Path -> PathFragment -> Tree -> Bool
    , canMove : Path -> Path -> Tree -> Bool
    , extractAt : Path -> Tree -> Maybe ( Tree, Tree )
    , fixPathForMovt : Path -> Path -> Path
    , isLastAt : Tree -> Path -> Bool
    }
internals = { allLast = allLast
            , canMove = canMove
            , isLastAt = isLastAt
            , extractAt = extractAt
            , fixPathForMovt = fixPathForMovt
            }

type alias TreeDatum = { text: Maybe String
                             -- TODO: we are relying on the code and not the
                             -- typechecker to maintain the invariant that
                             -- only terminal nodes have text
                       , label: String
                       , index: Maybe Index.Index
                       }

type alias Tree = T.Tree TreeDatum

-- TODO: Make get and friends (non-tail!) recursive: get p tree = getNthChild
-- (foot path) (get (leg path) tree)

-- Convenience functions for generating trees for testing
l : String -> String -> Tree
l label text = T.Tree { label = label
                      , text = Just text
                      , index = Nothing
                      }
               []

t : String -> List Tree -> Tree
t label children = T.Tree { label = label
                          , text = Nothing
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
        txt = d |> .text
    in
    case txt of
        Just _ -> t d
        Nothing -> nt d

get : Path -> Tree -> Maybe Tree
get path tree = case path of
                    Path.RootPath -> Just tree
                    Path.Path foot [] -> T.children tree |> flip (!!) foot
                    Path.Path foot (l :: ls) ->
                        (Path.Path l ls) |>
                        flip get tree ?>
                        T.children ?>?
                        flip (!!) foot

set : Path -> Tree -> Tree -> Maybe Tree
set path newChild tree = case path of
                             Path.RootPath -> Just newChild
                             Path.Path i [] -> TX.setChild i newChild tree
                             Path.Path foot (l :: ls) ->
                                 let
                                     parentPath = Path.Path l ls
                                     parent = get parentPath tree
                                 in
                                     parent ?>?
                                     TX.setChild foot newChild ?>?
                                     \x -> set parentPath x tree

do : Path.Path -> (Tree -> Tree) -> Tree -> Tree
do path f tree =
    Debug.crash "TODO"

highestIndex : Tree -> Int
highestIndex t =
    let
        fold : TreeDatum -> Int -> Int
        fold d i = Maybe.withDefault 0 (d.index ?> .number) |> max i
    in
        T.foldl fold 0 t

-- Movement

isFirstAt : Tree -> Path -> Bool
isFirstAt _ p =
    case p of
        Path.RootPath -> False
        Path.Path foot _ -> foot == 0

allFirst : Path -> PathFragment -> Tree -> Bool
allFirst path frag tree =
    Utils.all (isFirstAt tree) (Path.allCombos path frag)

isLastAt : Tree -> Path -> Bool
isLastAt tree path =
    Debug.log "tree" (get (Path.parent path) tree) ?>
    T.children |> Debug.log "kids" ?>
    List.length |> Debug.log "length" ?>
    (==) (Debug.log "target" (Path.foot path + 1)) |> Debug.log "result" |>
    Maybe.withDefault False |>
    Debug.log ("isLastAt " ++ toString path)

allLast : Path -> PathFragment -> Tree -> Bool
allLast path frag tree =
    Utils.all (isLastAt tree) (Path.allCombos path frag)

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
                newFrag = Path.moveLeft frag
            in allLast path newFrag

isOnlyChildAt : Tree -> Path -> Bool
isOnlyChildAt t p = isFirstAt t p && isLastAt t p

destPath : Path -> Path -> Tree -> Maybe Path
destPath src newParent tree =
    case Debug.log "rc" <| Path.splitCommon (Debug.log "src" src) (Debug.log "np" newParent) of
        -- Movement to own child disallowed
        (_, Path.PF [], _) -> Debug.log "whoops" Nothing
        -- Movement to own parent
        (_, Path.PF l, Path.PF []) -> case Debug.log "first/last"
                                      (isFirstAt tree src, isLastAt tree src)
                                      of
                                          -- Land to parent's left
                                          (True, False) -> List.Extra.last l |>
                                                           Utils.fromJust |>
                                                           List.Extra.singleton |>
                                                           Path.PF |>
                                                           Path.join newParent |>
                                                           Just
                                          -- Land to parent's right
                                          (False, True) -> List.Extra.last l |>
                                                           Utils.fromJust |>
                                                          (+) 1 |>
                                                          List.Extra.singleton |>
                                                          Path.PF |>
                                                          Path.join newParent |>
                                                          Just
                                          otherwise -> Nothing
        (common, _, dest) ->
            let
                rightward = Path.lessThan src newParent
                dest1 = Path.PF <| (if rightward
                                    then [0]
                                    else [ get newParent tree ?>
                                               T.children ?>
                                               List.length |>
                                               Utils.fromJust
                                         ])
                        ++ Path.unwrap dest
            in
                Just <| Path.join common dest1

-- It is allowed to move SRC to DEST without affecting the word order in the
-- tree if the following conditions are met: remove the common elementes on
-- the path from the root to SRC and the path from root to DEST.  Then, the
-- first elements on the remaining path should be immediately adjacent (in the
-- appropriate direction for the intended movement, i.e. S >> D for rightward
-- movt and vice versa).  And, the reamining elements on each path must be the
-- left/rightmost (appropriately valued for the path to SRC and DEST and
-- whether the movement is leftward or rightward)
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

extractAt : Path -> Tree -> Maybe (Tree, Tree)
extractAt path tree =
    case path of
        RootPath -> Nothing
        otherwise ->
            let
                -- TODO: all this fromJust cries out for a nonempty list type
                path1 = Path.parent path
                idx = Path.foot path
                child = get path tree
            in
                get path1 tree ?>
                TX.updateChildren (Utils.remove idx) ?>?
                (\x -> set path1 x tree) |>
                Maybe.map2 (,) child

insertAt : Path -> Tree -> Tree -> Maybe Tree
insertAt path newChild tree =
    case path of
        RootPath -> Just newChild
        otherwise ->
            let
                path1 = Path.parent path
                idx = Path.foot path
                child = get path tree
            in
                get path1 tree ?>
                TX.updateChildren (Utils.insert idx newChild) ?>?
                (\x -> set path1 x tree)

fixPathForMovt : Path -> Path -> Path
fixPathForMovt src dest =
    let
        (common, src1, dest1) = Path.splitCommon src dest
        rightward = Path.lessThan src dest
    in
        if rightward && Path.isFragSingleton src1
        then Path.join common <| Path.moveLeft dest1
        else dest

moveTo : Path -> Path -> Tree -> Maybe Tree
moveTo source dest tree =
    case canMove source dest tree of
        False -> Debug.log "can't move" Nothing
        True ->
            tree |>
            extractAt source |>
            Debug.log "extract" ?>?
            uncurry (insertAt (Debug.log "fpm" <| fixPathForMovt source dest)) |>
            Debug.log "insert"
