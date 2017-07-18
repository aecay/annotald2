module TreeEdit.Actions exposing ( clearSelection
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
import TreeEdit.Res as R

import Monocle.Optional as Optional

-- Third party

import List.Extra exposing (zip)

-- Annotald packages

import TreeEdit.Tree as Tree exposing (Tree)
import TreeEdit.Path as Path exposing (Path)
import TreeEdit.Utils as Utils exposing ((=>>), with)
import TreeEdit.Model as Model exposing (Model)
import TreeEdit.Selection as Selection
import TreeEdit.Index as Index exposing (normal, Variety(..))

type alias Result = R.Result Model

-- The rest of the file

type alias Action = Model -> Result

-- TODO: remove
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

coIndex : Model -> Result
coIndex model =
    let
        sel = model.selected
        one : Model -> Result
        one = (Selection.withOne sel coIndex1 R.succeed)
        two : Model -> Result
        two = (Selection.withTwo sel coIndex2 R.succeed)
    in
        R.succeed model |>
        R.andThen one |>
        R.andThen two

coIndex1: Path -> Model -> Result
coIndex1 = removeIndexAt

coIndex2 : Path -> Path -> Model -> Result
coIndex2 path1 path2 model =
    case Path.root path1 == Path.root path2 of
        False -> R.failWarn "Can't coindex nodes in two different roots"
        True ->
            let
                root = (.get Model.root model)
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
                    (Err _, Ok x) -> setIndexAt path1 x.number model
                    (Ok x, Err _) -> setIndexAt path2 x.number model
                    -- Both of the nodes have an index: toggle index type
                    (Ok x, Ok y) ->
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
                    (Err _, Err _) -> setIndexAt path1 ind model |>
                                                    R.andThen (setIndexAt path2 ind)

setIndexAt: Path -> Int -> Action
setIndexAt path index =
    Tree.index =>> Index.number |>
    .set |>
    with index |>
    doAt path

setIndexVarietyAt : Path -> Index.Variety -> Action
setIndexVarietyAt path newVariety =
    doAt path ((Optional.composeLens Tree.index Index.variety |> .set) newVariety)

removeIndexAt : Path -> Action
removeIndexAt path =
    let
        f x = { x | index = Nothing }
    in
        doAt path Tree.removeIndex

incrementIndicesBy : Int -> Path -> Tree -> R.Result Tree
incrementIndicesBy inc path tree =
    Optional.modify (Tree.index =>> Index.number) ((+) inc) |>
    Tree.map |>
    (\x -> Tree.do path x tree)

doMove : Path -> Path -> Model -> Result
doMove src dest model =
    let
        dest1 = Tree.destPath src dest (.get Model.root model)
        newSel = dest1 |>
                 R.map (Tree.fixPathForMovt src >> Selection.one)
        srcRoot = Path.root src
        destRoot = Path.root dest
        newRoot = if srcRoot == destRoot
                  then R.succeed (.get Model.root model)
                  else
                      let
                          inc = (Tree.get destRoot (.get Model.root model)) |> R.map Tree.highestIndex
                      in
                          inc |>
                          R.andThen (\x -> incrementIndicesBy x srcRoot (.get Model.root model))

    in
        -- TODO: make moveTo (or a new fn) do the calculation of the dest
        -- path, so we don't have to worry about it here.
        R.map2 (\x y -> (x, y)) newRoot dest1 |>
        R.andThen (\(nr, d) -> Tree.moveTo src d nr) |>
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
        trace = Tree.get src (.get Model.root model) |> R.map Tree.makeTrace
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
    Selection.withOne model.selected (\x -> (leafBeforeInner newLeaf x (.get Model.root model)) |>
                                          R.map (\x -> (.set Model.root) x model)) |>
    -- TODO: test
    Selection.withTwo model.selected (doMovement model)


deleteNode : Model -> Result
deleteNode model =
    let
        delete : Path -> R.Result Tree
        delete path =
            let
                node = Tree.get path (.get Model.root model)
                isTerminal = node |> R.map Tree.isTerminal
                -- We make these functions of one ignored argument
                -- (i.e. thunks) to avoid computing them both
                deleteTerminal _ =
                    let
                        isNonEmpty = node |> R.map (Tree.isEmpty >> not)
                        isOnlyChild = path |>
                                      Path.parent |>
                                      flip Tree.get (.get Model.root model) |>
                                      R.map (.getOption Tree.children) |>
                                      R.map (\x -> Maybe.withDefault False <| Maybe.map (List.length >> (==) 1) x)
                    in
                        R.ifThen isOnlyChild (R.fail "Cannot delete an only child") <|
                            R.ifThen isNonEmpty (R.fail "Cannot delete a non-empty terminal") <|
                            (Tree.extractAt path (.get Model.root model) |> R.map Tuple.second)
                deleteNonTerminal _ =
                    let
                        kids = node |> R.map (.getOption Tree.children) |> R.map Utils.fromJust
                        newRoot = Tree.extractAt path (.get Model.root model) |> R.map Tuple.second
                    in
                        R.foldr (Tree.insertAt path) newRoot kids
            in
                R.ifThen isTerminal
                    (deleteTerminal ())
                    (deleteNonTerminal ())
    in
        R.succeed model |>
        Selection.withOne model.selected (delete >> R.map (flip (.set Model.root) model)) |>
        R.andThen clearSelection
