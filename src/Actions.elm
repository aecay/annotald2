module Actions exposing ( clearSelection
                        , changeLabel
                        , coIndex
                        , Action
                        , doMove
                        , createParent
                        , deleteNode
                        , leafBefore
                        , leafBeforeInner -- For ContextMenu
                        )

-- This module contains the types and functions for creating *actions*, or
-- functions that respond to user input.

-- Standard library

import Maybe exposing (withDefault)
import Dict exposing (Dict)
import Res as R
import Result

import Monocle.Optional as Optional

-- Third party

import List.Extra exposing (zip)

-- Annotald packages

import Tree exposing (Tree)
import Path exposing (Path)
import Utils
import Model exposing (Model)
import Selection
import Index exposing (normal, Variety(..))

type alias Result = R.Result Model

-- The rest of the file

type alias Action = Model -> Result

doOneSelected : (Tree -> Tree) -> Model -> Result
doOneSelected f model =
    let
        path = model.selected |>
               Selection.first
    in R.lift "doOneSelected" path |>
    R.andThen (\x -> doAt x f model)

doAt : Path -> (Tree -> Tree) -> Model -> Result
doAt path f model =
    -- TODO: ideally doRoot returns a Result, meaning that R.modify does...
    R.succeed <| Model.doRoot (Tree.do path f) model

clearSelection : Action
clearSelection m = R.succeed { m | selected = Selection.empty }

changeLabel : List String -> Action
changeLabel labels =
    case labels of
        [] -> R.succeed
        head :: tail ->
            let
                pairs : List (String, String)
                pairs = tail ++ [head] |> zip (head :: tail)
                repls : Dict String String
                repls = Dict.fromList pairs
                change : String -> String
                change s = Dict.get s repls |> withDefault head
                update : Tree -> Tree
                update z = (\d -> { d | label = change d.label }) z
            in doOneSelected update

coIndex : Action
coIndex model =
    let
        sel = model.selected
        first = Selection.first sel
        second = Selection.second sel
    in
        case (first, second) of
            (Just f, Just s) ->
                coIndex2 f s model
            (Just f, Nothing) -> coIndex1 f model
            otherwise -> R.fail "coIndex"

coIndex1: Path -> Model -> Result
coIndex1 = removeIndexAt

coIndex2 : Path -> Path -> Model -> Result
coIndex2 path1 path2 model =
    case Path.root path1 == Path.root path2 of
        False -> R.failWarn "Can't coindex nodes in two different roots"
        True ->
            let
                root = model.root
                tree1 = Tree.get path1 root
                tree2 = Tree.get path2 root
                index1 = tree1 |> R.map (.getOption Tree.index) |> R.andThen (R.lift "index1")
                index2 = tree2 |> R.map (.getOption Tree.index) |> R.andThen (R.lift "index2")
                ind = Tree.get (Path.root path1) root |>
                      R.map Tree.highestIndex |>
                      R.withDefault 0 |>
                      (+) 1
            in
                case (index1, index2) of
                    -- One of the nodes has an index, the other does not: set
                    -- the index of the unindexed node to match
                    (Result.Err _, Result.Ok x) -> setIndexAt path1 x.number model
                    (Result.Ok x, Result.Err _) -> setIndexAt path2 x.number model
                    -- Both of the nodes have an index: toggle index type
                    (Result.Ok x, Result.Ok y) ->
                        case (x.variety, y.variety) of
                            -- Normal coindexing -> gap
                            (Index.Normal, Index.Normal) ->
                                setIndexVarietyAt path2 Index.Gap model
                            -- Gap -> backwards gap
                            (Index.Normal, Index.Gap) ->
                                setIndexVarietyAt path1 Index.Gap model |>
                                R.andThen (setIndexVarietyAt path2 Index.Normal)
                            -- Backwards gap -> remove indexes
                            (Index.Gap, Index.Normal) ->
                                removeIndexAt path1 model |>
                                R.andThen (removeIndexAt path2)
                            -- Something weird -> remove indexes (TODO: is
                            -- this right?)
                            otherwise -> removeIndexAt path1 model |>
                                         R.andThen (removeIndexAt path2)
                    -- Neither node has an index -> coindex them
                    (Result.Err _, Result.Err _) -> setIndexAt path1 ind model |>
                                                    R.andThen (setIndexAt path2 ind)

-- TODO: remove; trivial
setIndexAt: Path -> Int -> Action
setIndexAt path index =
        doAt path (.set (Optional.composeLens Tree.index Index.number) index)

setIndexVarietyAt : Path -> Index.Variety -> Action
setIndexVarietyAt path newVariety =
    doAt path ((Optional.composeLens Tree.index Index.variety |> .set) newVariety)

removeIndexAt : Path -> Action
removeIndexAt path =
    let
        f x = { x | index = Nothing }
    in
        doAt path Tree.removeIndex

-- TODO: update the trace indices if we move one tree into another
doMove : Path -> Path -> Model -> Result
doMove src dest model =
    let
        -- rightward will have a bogus value if src is Nothing, but in that
        -- case we're going to fail when we try to lift it below, so it
        -- doesn't matter
        rightward = Path.lessThan src dest
        dest1 = Tree.destPath src dest model.root
        newSel = dest1 |>
                 R.map (Tree.fixPathForMovt src) |>
                 R.map Selection.one
    in
        -- TODO: make moveTo (or a new fn) do the calculation of the dest
        -- path, so we don't have to worry about it here.
        dest1 |>
        R.andThen (\x -> Tree.moveTo src x model.root) |>
        R.map ((flip (.set Model.root)) model) |>
        R.map2 (\s m -> { m | selected = s }) newSel


createParent2 : String -> Path -> Path -> Model -> Result
createParent2 label one two model =
    let
        parent1 = Path.parent one
        parent2 = Path.parent two
        (foot1, foot2) = Utils.sort2 (Path.foot one) (Path.foot two)
    in
        if parent1 /= parent2
        then R.fail "parents are different for createParent2"
        else doAt parent1 (Tree.updateChildren (\c -> let (x, y, z) = Utils.splice foot1 (foot2+1) c
                                                      in x ++ [Tree.t label y] ++ z))
            model

createParent : String -> Model -> Result
createParent label model =
    let
        none : Model -> Result
        none = \x -> R.succeed x
        one : Path -> Model -> Result
        one path = doAt path (\x -> Tree.t label [x])
    in
        model |> Selection.perform model.selected none one (createParent2 label)

doMovement : Model -> Path -> Path -> Result
doMovement model src dest =
    let
        trace = Tree.get src model.root |> R.map Tree.makeTrace
    in
        R.andThen (\x -> leafBefore x model) trace |>
        R.andThen (coIndex2 src dest)

leafBeforeInner : Tree -> Path -> Tree -> R.Result Tree
leafBeforeInner newLeaf path tree =
    let
        parent = Path.parent path
        foot = Path.foot path
        update c = List.take foot c ++ [newLeaf] ++ List.drop foot c
    in
        Tree.do parent (Tree.updateChildren update) tree

leafBefore : Tree -> Model -> Result
leafBefore newLeaf model =
    R.succeed model |>
    Selection.withOne model.selected (\x -> (leafBeforeInner newLeaf x model.root) |>
                                          R.map (\x -> (.set Model.root) x model)) |>
    -- TODO: test
    Selection.withTwo model.selected (doMovement model)


deleteNode : Model -> Result
deleteNode model =
    let
        delete : Path -> R.Result Tree
        delete path =
            let
                node = Tree.get path model.root
                isTerminal = node |> R.map Tree.isTerminal
                -- We make these functions of one ignored argument
                -- (i.e. thunks) to avoid computing them both
                deleteTerminal _ =
                    let
                        isNonEmpty = node |> R.map (Tree.isEmpty >> not)
                        isOnlyChild = path |> Path.parent |> flip Tree.get model.root |> R.map (Tree.children >> List.length >> (==) 1)
                    in
                        R.ifThen isOnlyChild (R.fail "Cannot delete an only child") <|
                            R.ifThen isNonEmpty (R.fail "Cannot delete a non-empty terminal") <|
                            (Tree.extractAt path model.root |> R.map Tuple.second)
                deleteNonTerminal _ =
                    let
                        kids = node |> R.map (Tree.children)
                        newRoot = Tree.extractAt path model.root |> R.map Tuple.second
                    in
                        R.foldr (Tree.insertAt path) newRoot kids
            in
                R.ifThen isTerminal
                    (deleteTerminal ())
                    (deleteNonTerminal ())
    in
        R.succeed model |>
        Selection.withOne model.selected (delete >> R.map (flip (.set Model.root) model))
