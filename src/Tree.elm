module Tree exposing (l, t, Tree, either, Index,
                          IndexVariety(..),
                          -- TODO: exporting all this internal stuff is not
                          -- the best...
                          TreeDatum, get, Path, set,
                          highestIndex
                     --, sameRoot
                     , root
                     , moveTo
                     , internals
                     , destPath
                     , insertAt
                     , fixPathForMovt -- TODO: marginal on exporting this
                     )

import List
import List.Extra exposing ((!!))
import Maybe

import Utils exposing ((?>?), (?>))

import MultiwayTree as T
import TreeExts as TX

-- Functions we expose for testing only
internals :
    { allLast : Path -> PathFragment -> Tree -> Bool
    , canMove : Path -> Path -> Tree -> Bool
    , extractAt : Path -> Tree -> Maybe ( Tree, Tree )
    , fixPathForMovt : Path -> Path -> Path
    , isLastAt : Tree -> Path -> Bool
    , removeCommon : Path -> Path -> ( Path, PathFragment, PathFragment )
    }
internals = { allLast = allLast
            , canMove = canMove
            , isLastAt = isLastAt
            , removeCommon = removeCommon
            , extractAt = extractAt
            , fixPathForMovt = fixPathForMovt
            }

type IndexVariety = Normal | Gap

type alias Index = { number: Int
                   , variety: IndexVariety
                   }

type alias TreeDatum = { text: Maybe String
                             -- TODO: we are relying on the code and not the
                             -- typechecker to maintain the invariant that
                             -- only terminal nodes have text
                       , label: String
                       , index: Maybe Index
                       }

type alias Tree = T.Tree TreeDatum

type alias Path = List Int
-- TODO: paths should have a foot (last elt) and leg (everything else).  Make
-- get and friends (non-tail!) recursive:
-- get p tree = getNthChild (foot path) (get (leg path) tree)

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
                    [] -> Just tree
                    i :: is -> T.children tree |> flip (!!) i |> Maybe.andThen (get is)

set : Path -> Tree -> Tree -> Maybe Tree
set path newChild tree = case path of
                             [] -> Just newChild
                             i :: [] -> TX.setChild i newChild tree
                             i :: is -> (T.children tree) !! i ?>?
                                        set is newChild ?>?
                                        \x -> TX.setChild i x tree

root : Path -> Path
root a = [Utils.fromJust (a !! 0)]

highestIndex : Tree -> Int
highestIndex t =
    let
        fold : TreeDatum -> Int -> Int
        fold d i = Maybe.withDefault 0 (d.index ?> .number) |> max i
    in
        T.foldl fold 0 t

-- Movement

-- TODO: use the typechecker to guarantee that we never use a fragment as a
-- path in dumb ways
-- type PathFragment = Fragment Path
type alias PathFragment = List Int

-- For when we use the typechecker as above
-- joinFragment : Path -> PathFragment -> Path
-- joinFragment path frag =
--     path ++ frag

removeCommon : Path -> Path -> (Path, PathFragment, PathFragment)
removeCommon p1 p2 =
    let
        go p1 p2 accum = case (p1, p2) of
                             (h1 :: t1, h2 :: t2) -> if h1 == h2
                                                     then go t1 t2 <| accum ++ [h1]
                                                     else (accum, p1, p2)
                             otherwise -> (accum, p1, p2)
    in
        go p1 p2 []

-- isParent : Path -> Path -> Bool
-- isParent parent child =
--     let
--         (_, _, x) = removeCommon

isFirstAt : Tree -> Path -> Bool
isFirstAt _ p =
    List.Extra.last p == Just 0

allFirst : Path -> PathFragment -> Tree -> Bool
allFirst _ frag _ =
    Utils.all ((==) 0) frag

isLastAt : Tree -> Path -> Bool
isLastAt tree path =
    case path of
        [] -> True
        x :: [] -> (List.length (T.children tree)) == (x + 1)
        otherwise ->
            let
                path1 = List.Extra.init path |> Utils.fromJust
                tail = List.Extra.last path |> Utils.fromJust
                test : Tree -> Bool
                test x = List.length (T.children x) == tail + 1
            in
                get path1 tree ?>
                test |>
                Maybe.withDefault False

allLast : Path -> PathFragment -> Tree -> Bool
allLast path frag tree =
    let
        paths : List Path
        paths = List.Extra.inits frag |>
                -- Remove the empty list
                List.tail |>
                Utils.fromJust |>
                List.map ((++) path)
    in
        Utils.all (isLastAt tree) paths

allLastForDest : Path -> PathFragment -> Tree -> Bool
allLastForDest path frag =
    case path of
        [] -> \_ -> True
        otherwise ->
            let
                -- When speaking of a movement destination, the last element
                -- of frag should actually be pointing one after the end of
                -- the list, so we retract it by one here to simplify the
                -- testing logic in allLast
                newFrag = (List.Extra.init frag |> Utils.fromJust) ++
                          (List.Extra.last frag |> Utils.fromJust |> (\x -> x - 1) |>
                               List.Extra.singleton)
            in allLast path newFrag

isOnlyChildAt : Tree -> Path -> Bool
isOnlyChildAt t p = isFirstAt t p && isLastAt t p

destPath : Path -> Path -> Tree -> Maybe Path
destPath src newParent tree =
    case Debug.log "rc" <| removeCommon (Debug.log "src" src) (Debug.log "np" newParent) of
        -- Movement to own child disallowed
        (_, [], _) -> Debug.log "whoops" Nothing
        -- Movement to own parent
        (_, p :: _, []) -> case Debug.log "first/last" (isFirstAt tree src, isLastAt tree src) of
                               -- Land to parent's left
                               (True, False) -> Just <| newParent ++ [p]
                               -- Land to parent's right
                               (False, True) -> Just <| newParent ++ [p+1]
                               otherwise -> Nothing
        (common, _, dest) ->
            let
                rightward = src < newParent
                dest1 = dest ++
                        if rightward
                        then [0]
                        else [get newParent tree ?> T.children ?> List.length |> Utils.fromJust]
            in
                Just <| common ++ dest1

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
        (common, src1, dest1) = removeCommon src dest
        rightward = src < dest
        testFnSrc = if rightward then allLast else allFirst
        testFnDest = if rightward then allFirst else allLastForDest
    in
        Debug.log (toString (common, src1, dest1)) <|
        case (src1, dest1) of
            ((hsrc :: tsrc), (hdest :: tdest)) ->
                Debug.log "a" (if rightward
                 then hsrc == hdest - 1
                 else hsrc == hdest + 1) &&
                Debug.log "b" (testFnSrc (common ++ [hsrc]) tsrc tree) &&
                Debug.log "c" (testFnDest (common ++ [hdest]) tdest tree) &&
                Debug.log "d" (not (isOnlyChildAt tree src))
            ((hsrc :: tsrc), []) ->
                -- Movement to parent leftward
                if rightward
                then False
                else Debug.log "b'" (testFnSrc (common ++ [hsrc]) tsrc tree) &&
                Debug.log "d'" (not (isOnlyChildAt tree src))

            otherwise -> False

extractAt : Path -> Tree -> Maybe (Tree, Tree)
extractAt path tree =
    case path of
        [] -> Nothing
        otherwise ->
            let
                -- TODO: all this fromJust cries out for a nonempty list type
                path1 = List.Extra.init path |> Utils.fromJust
                idx = List.Extra.last path |> Utils.fromJust
                child = get path tree
            in
                get path1 tree ?>
                TX.updateChildren (Utils.remove idx) ?>?
                (\x -> set path1 x tree) |>
                Maybe.map2 (,) child

insertAt : Path -> Tree -> Tree -> Maybe Tree
insertAt path newChild tree =
    case path of
        [] -> Nothing
        otherwise ->
            let
                path1 = List.Extra.init path |> Utils.fromJust
                idx = List.Extra.last path |> Utils.fromJust
                child = get path tree
            in
                get path1 tree ?>
                TX.updateChildren (Utils.insert idx newChild) ?>?
                (\x -> set path1 x tree)

fixPathForMovt : Path -> Path -> Path
fixPathForMovt src dest =
    let
        (common, src1, dest1) = removeCommon src dest
        rightward = src < dest
    in
        if rightward && List.length src1 == 1
        then common ++ (List.head dest1 ?>
                            flip (-) 1 |>
                            Maybe.map2 (flip (::)) (List.tail dest1) |>
                            Maybe.withDefault [])
        else dest

moveTo : Path -> Path -> Tree -> Maybe Tree
moveTo source dest tree =
    case canMove source dest tree of
        False -> Debug.log "can't move" Nothing
        True ->
            tree |>
            extractAt source |>
            Debug.log "extract" ?>?
            uncurry (insertAt (fixPathForMovt source dest)) |>
            Debug.log "insert"
