module TreeEdit.Actions exposing ( clearSelection
                                 , changeLabel
                                 , coIndex
                                 , Action
                                 , doMove
                                 , createParent
                                 , deleteNode
                                 , leafBefore
                                 , leafBeforeInner -- For ContextMenu
                                 , leafAfter
                                 , finishLabelEdit
                                 , editLabel
                        )

-- This module contains the types and functions for creating *actions*, or
-- functions that respond to user input.

-- Standard library

import Maybe exposing (withDefault)
import Dict exposing (Dict)
import TreeEdit.Result as R exposing (Result(..))

import Monocle.Optional as Optional

-- Third party

import List.Extra exposing (zip)

-- Annotald packages

import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Path as Path exposing (Path)
import TreeEdit.Utils as Utils
import TreeEdit.Model as Model
import TreeEdit.Model.Type exposing (Model)
import TreeEdit.Selection as Selection
import TreeEdit.Index as Index exposing (normal, Variety(..))
import TreeEdit.View.LabelEdit as LabelEdit

type alias Result = R.Result Model

-- The rest of the file

type alias Action = Model -> Result

-- TODO: remove
doOneSelected : (Tree -> Tree) -> Model -> Result
doOneSelected f model =
    let
        path = model.selected |>
               (R.lift "no single selection" Selection.first)
    in
        path |>
        R.andThen (\x -> doAt x f model)

doAt : Path -> (Tree -> Tree) -> Model -> Result
doAt path f model =
    R.modify Model.root (Tree.do path f) model

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
                index1 = tree1 |> R.map (.getOption Tree.index)
                index2 = tree2 |> R.map (.getOption Tree.index)
                ind = Tree.get (Path.root path1) root |>
                      R.map Tree.highestIndex |>
                      R.withDefault 0 |>
                      (+) 1
                helper : Maybe Index.Index -> Maybe Index.Index -> Result
                helper i1 i2 =
                    case (i1, i2) of
                        -- One of the nodes has an index, the other does not: set
                        -- the index of the unindexed node to match
                        (Nothing, Just x) -> setIndexAt path1 x.number model
                        (Just x, Nothing) -> setIndexAt path2 x.number model
                        -- Both of the nodes have an index: toggle index type
                        (Just x, Just y) ->
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
                        (Nothing, Nothing) -> setIndexAt path1 ind model |>
                                              R.andThen (setIndexAt path2 ind)
            in
                R.andThen2 helper index1 index2

setIndexAt: Path -> Int -> Action
setIndexAt path index =
    doAt path ((Optional.composeLens Tree.index Index.number |> .set) index)

setIndexVarietyAt : Path -> Index.Variety -> Action
setIndexVarietyAt path newVariety =
    doAt path ((Optional.composeLens Tree.index Index.variety |> .set) newVariety)

removeIndexAt : Path -> Action
removeIndexAt path =
    doAt path Tree.removeIndex

incrementIndicesBy : Int -> Path -> Tree -> R.Result Tree
incrementIndicesBy inc path tree =
    Optional.modify (Optional.composeLens Tree.index Index.number) ((+) inc) |>
    Tree.map |>
    (\x -> Tree.do path x tree)

doMove : Path -> Path -> Model -> Result
doMove src dest model =
    let
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
        res = R.andThen3 Tree.moveTo (R.succeed src) (R.succeed dest) newRoot
        newRoot1 = R.map Tuple.first res
        newSel = R.map Tuple.second res
    in
        R.modify Model.root (always newRoot1) model |>
        R.andThen (R.modify Model.selected (always <| R.map Selection.one newSel))

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
            model |> R.map ((.set Model.selected) (Selection.one one))

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
    let
        createLeaf path = R.modify Model.root (leafBeforeInner newLeaf path) model
    in
    Selection.perform model.selected
        (R.succeed model)
        (createLeaf)
        (doMovement model) -- TODO: test

leafAfter : Tree -> Model -> Result
leafAfter newLeaf model =
    let
        createLeaf path = R.modify Model.root (leafBeforeInner newLeaf (Path.advance path)) model
    in
    Selection.perform model.selected
        (R.succeed model)
        (createLeaf)
        (\_ _ -> R.succeed model) -- TODO: get movement for this case working


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
                        isEmpty = node |> R.map (Tree.isEmpty)
                        hasSiblings = path |>
                                      Path.parent |>
                                      flip Tree.get (.get Model.root model) |>
                                      R.map (Maybe.withDefault [] << .getOption Tree.children) |>
                                      R.map (\x -> List.length x >= 1)
                    in
                        Tree.extractAt path (.get Model.root model) |>
                        R.map Tuple.second |>
                        R.guard hasSiblings "Cannot delete an only child" |>
                        R.guard isEmpty "Cannot delete a non-empty terminal"

                deleteNonTerminal _ =
                    let
                        kids = node |> R.map (.getOption Tree.children) |> R.map Utils.fromJust
                        newRoot = Tree.extractAt path (.get Model.root model) |> R.map Tuple.second
                    in
                        R.andThen2 (Tree.insertManyAt path) kids newRoot
                do x = if x
                       then deleteTerminal ()
                       else deleteNonTerminal ()
            in
                isTerminal |>
                R.andThen do
    in
        R.succeed model |>
        Selection.withOne model.selected (delete >> R.map (flip (.set Model.root) model)) |>
        R.andThen clearSelection


editLabel : Model -> Result
editLabel model =
    let
        -- TODO: don't want a second selection
        selected : R.Result Path.Path
        selected = model |> .get Model.selected |> Selection.first |> R.liftVal "nothing selected"
        root : R.Result Tree
        root = model |> .get Model.root |> R.succeed
        label : R.Result String
        label = R.andThen2 Tree.get selected root |> R.map .label
        initForm = R.map LabelEdit.init label
    in
        R.map (\l -> { model | labelForm = Just <| LabelEdit.init l}) label

finishLabelEdit : Model -> Result
finishLabelEdit model =
    let
        selected = model |> .get Model.selected |> Selection.first |> R.liftVal "nothing selected"
        root = model |> .get Model.root
        newLabel = model.labelForm |> R.liftVal "not editing" |> R.andThen LabelEdit.finish
        changeLabel = newLabel |> R.map (\label -> (\tree -> { tree | label = label }))
    in
        R.andThen3 doAt selected changeLabel (R.succeed model) |>
        R.map (\m -> { m | labelForm = Nothing })
