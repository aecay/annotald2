module Actions exposing ( clearSelection
                        , changeLabel
                        , coIndex
                        , Action
                        , doMove
                        , createParent
                        )

-- This module contains the types and functions for creating *actions*, or
-- functions that respond to user input.

-- Standard library

import Maybe exposing (withDefault)
import Dict exposing (Dict)
import Res as R
import Result

-- Third party

import MultiwayTree as MT
import List.Extra exposing (zip)

-- Annotald packages

import Tree exposing (Tree)
import Path exposing (Path)
import Utils
import TreeExts as TX
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
                update z = TX.updateDatum (\d -> { d | label = change d.label }) z
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
                index1 = tree1 |> R.map (MT.datum >> .index) |> R.andThen (R.lift "index1")
                index2 = tree2 |> R.map (MT.datum >> .index) |> R.andThen (R.lift "index2")
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

setIndexAt: Path -> Int -> Action
setIndexAt path index =
    let
        f x = { x | index = Just <| Index.normal index }
    in
        doAt path <| TX.updateDatum f

setIndexVarietyAt : Path -> Index.Variety -> Action
setIndexVarietyAt path newVariety =
    let
        setVariety x = { x | variety = newVariety }
        f x = { x | index = Maybe.map setVariety x.index }
    in
        doAt path (TX.updateDatum f)

removeIndexAt : Path -> Action
removeIndexAt path =
    let
        f x = { x | index = Nothing }
    in
        doAt path (TX.updateDatum f)

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
        else doAt parent1 (TX.updateChildren (\c -> let (x, y, z) = Utils.splice foot1 (foot2+1) c
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

leafBefore : String -> String -> Model -> Result
leafBefore label text model =
    let
        do path =
            let
                parent = Path.parent path
                foot = Path.foot path
                update c = List.take foot c ++ [Tree.l label text] ++ List.drop foot c
            in
                doAt parent (TX.updateChildren update)
        quit _ = R.fail "leafBefore"
        quit2 _ _ _ = R.fail "leafBefore" -- TODO: magic movement trace creator
    in
        model |> Selection.perform model.selected quit do quit2

deleteNode : Model -> Result
deleteNode model =
    let
        delete path =
            let
                node = Tree.get path model.root
                isTerminal = node |> R.map Tree.isTerminal
            in
                Debug.crash "foo" -- TODO
    in
        Debug.crash "bar"
