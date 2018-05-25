module TreeEdit.Action exposing ( clearSelection
                                , changeLabel
                                , coIndex
                                , Action
                                , doMove
                                , createParent
                                , deleteNode
                                , leafBefore
                                , leafBeforeInner -- For ContextMenu TODO:
                                                  -- consolidate with createLeaf
                                , leafAfter
                                , finishLabelEdit
                                , editLabel
                                , toggleDashTag
                                , undo
                                , redo
                        )

-- This module contains the types and functions for creating *actions*, or
-- functions that respond to user input.

-- Standard library

import Dom
import Maybe exposing (withDefault)
import Dict exposing (Dict)
import TreeEdit.Result as R exposing (Result(..))
import Task
import Monocle.Lens as Lens
import Monocle.Optional as Optional exposing (fromLens)
import Monocle.Common exposing ((=>), maybe)

-- Third party

import List.Extra exposing (zip)

-- Annotald packages

import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type as TreeType exposing (Tree)
import TreeEdit.Path as Path exposing (Path)
import TreeEdit.Utils as Utils exposing (maybeAndThen2, o)
import TreeEdit.Model as Model
import TreeEdit.Model.Type exposing (Model)
import TreeEdit.Msg exposing (Msg(Undo, Redo))
import TreeEdit.Selection as Selection
import TreeEdit.Index as Index exposing (normal, Variety(..))
import TreeEdit.View.LabelEdit as LabelEdit
import TreeEdit.Msg as Msg

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
    R.modify Model.root (Tree.do path f >> R.liftVal "doAt") model

clearSelection : Action
clearSelection m = R.succeed { m | selected = Selection.empty }

changeLabel : List String -> Model -> Result
changeLabel labels model =
    let
        selected = model |> .get Model.selected |> Selection.getOne
    in
        case selected of
            Just sel ->
                if Tree.get sel (.get Model.root model)
                    |> Maybe.map Tree.hasTerminalLabel
                    |> Maybe.withDefault True
                then R.succeed model
                else
                    case labels of
                        [] -> R.succeed model
                        head :: tail ->
                            let
                                pairs : List (String, String)
                                pairs = tail ++ [head] |> zip (head :: tail)
                                repls : Dict String String
                                repls = Dict.fromList pairs
                                change : String -> String
                                change s = Dict.get s repls |> withDefault head
                                update : Tree -> Tree
                                update = Lens.modify Tree.label change
                            in
                                doAt sel update model
            Nothing -> R.succeed model

coIndex : Model -> Result
coIndex model =
    Selection.perform model.selected
        (R.succeed model)
        (coIndex1 model)
        (coIndex2 model)

coIndex1: Model -> Path -> Result
coIndex1 = flip removeIndexAt

coIndex2 : Model -> Path -> Path -> Result
coIndex2 model path1 path2  =
    case Path.root path1 == Path.root path2 of
        False -> R.failWarn "Can't coindex nodes in two different roots"
        True ->
            let
                root = (.get Model.root model)
                tree1 = Tree.get path1 root
                tree2 = Tree.get path2 root
                index1 = tree1 |> Maybe.andThen (.get Tree.index)
                index2 = tree2 |> Maybe.andThen (.get Tree.index)
                ind = Tree.get (Path.root path1) root |>
                      Maybe.map Tree.highestIndex |> -- TODO: I think this
                                                     -- logic is duplicated
                                                     -- elsewhere in this file
                      Maybe.withDefault 0 |>
                      (+) 1
                helper : Maybe Index.Index -> Maybe Index.Index -> Result
                helper i1 i2 =
                    case (i1, i2) of
                        -- One of the nodes has an index, the other does not: set
                        -- the index of the unindexed node to match
                        (Nothing, Just x) -> setIndexAt path1 x.number model
                        (Just x, Nothing) -> setIndexAt path2 x.number model
                        -- Both of the nodes have an index: toggle index type
                        -- TODO: must check that indices are both equal
                        (Just x, Just y) ->
                            case Debug.log "both" (x.variety, y.variety) of
                                -- Normal coindexing -> gap
                                (Index.Normal, Index.Normal) ->
                                    setIndexVarietyAt path2 Index.Gap model |>
                                    R.andThen (\x -> if isGapAt path2 x
                                                     then R.succeed x
                                                     else setIndexVarietyAt path1 Index.Gap model)
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
                helper index1 index2

setIndexAt: Path -> Int -> Action
setIndexAt path index =
    doAt path (.set Tree.index <| Just <| Index.normal index)

setIndexVarietyAt : Path -> Index.Variety -> Action
setIndexVarietyAt path newVariety =
    doAt path (((o Tree.index) => maybe => (o Index.variety) |> .set) newVariety)

isGapAt : Path -> Model -> Bool
isGapAt path model =
    .get Model.root model |>
    Tree.get path |>
    Maybe.andThen (.getOption <|
                       (o Tree.index) => maybe => (o Index.variety)) |>
    Maybe.withDefault Index.Normal |>
    (==) Index.Gap

removeIndexAt : Path -> Action
removeIndexAt path =
    doAt path (.set Tree.index Nothing)

incrementIndicesBy : Int -> Path -> Tree -> Tree
incrementIndicesBy inc path tree =
    Optional.modify ((o Tree.index) => maybe => (o Index.number)) ((+) inc) |>
    Tree.map |>
    (\x -> Tree.do path x tree) |>
    Maybe.withDefault tree

doMove : Path -> Path -> Model -> Result
doMove src dest model =
    let
        srcRoot = Path.root src
        destRoot = Path.root dest
        newRoot = if srcRoot == destRoot
                  then .get Model.root model
                  else
                      let
                          inc = (Tree.get destRoot (.get Model.root model)) |>
                                Maybe.map Tree.highestIndex |>
                                Maybe.withDefault 0
                      in
                          incrementIndicesBy inc srcRoot (.get Model.root model)
        res = Tree.moveTo src dest newRoot
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
                                                      in x ++ [.t TreeType.private label y] ++ z))
            model |> R.map ((.set Model.selected) (Selection.one <| Path.childPath foot1 parent1))

createParent : String -> Model -> Result
createParent label model =
    let
        one path = doAt path (\x -> .t TreeType.private label [x])
    in
        model |> Selection.perform model.selected R.succeed one (createParent2 label)

doMovement : Model -> Path -> Path -> Result
doMovement model dest src =
    let
        root = (.get Model.root) model
        indDef = root |>
                 Tree.get (Path.root src) |>
                 Maybe.map Tree.highestIndex |>
                 Maybe.withDefault 0 |>
                 (+) 1
        ind = root |>
              Tree.get src |>
              Maybe.andThen (.get Tree.index) |>
              Maybe.map (.get Index.number) |>
              Maybe.withDefault indDef
        m = doAt src (.set Tree.index <| Just <| Index.normal ind) model
        -- TODO: can get the trace from the passed in node, no need to
        -- separately pass the index
        trace : R.Result Tree
        trace = m |>
                R.map (.get Model.root) |>
                R.andThen (Tree.get src >> R.liftVal "doMovement") |>
                R.map (flip Tree.makeTrace ind)
        sameRoot = Path.root src == Path.root dest
    in
        if sameRoot
        then R.andThen3 createLeaf trace m (R.succeed dest) |>
             R.map (.set Model.selected <| Selection.one dest)
        else R.fail "Can't make movement trace across different root nodes"

leafBeforeInner : Tree -> Path -> Tree -> R.Result Tree
leafBeforeInner newLeaf path tree =
    let
        parent = Path.parent path
        foot = Path.foot path
        update c = List.take foot c ++ [newLeaf] ++ List.drop foot c
    in
        Tree.do parent (Tree.updateChildren update) tree |>
        R.liftVal "leafBeforeInner"

createLeaf : Tree -> Model -> Path -> Result
createLeaf leaf m path = R.modify Model.root (leafBeforeInner leaf path) m

leafBefore : Tree -> Model -> Result
leafBefore newLeaf model =
    Selection.perform model.selected
        (R.succeed model)
        (createLeaf newLeaf model)
        (doMovement model)

leafAfter : Tree -> Model -> Result
leafAfter newLeaf model =
    Selection.perform model.selected
        (R.succeed model)
        ((createLeaf newLeaf model) << Path.advance)
        (\dest src -> doMovement model (Path.advance dest) src)


deleteNode : Model -> Result
deleteNode model =
    let
        root = .get Model.root model
        delete : Path -> R.Result Tree
        delete path =
            let
                node = Tree.get path root
            in
                case node of
                    Nothing -> R.fail "deleteNode"
                    Just n ->
                        case .get Tree.children n of
                            [] ->
                                let
                                    isEmpty = Tree.isEmpty n
                                    hasSiblings = path |>
                                                  Path.parent |>
                                                  flip Tree.get root |>
                                                  Maybe.map (.get Tree.children) |>
                                                  Maybe.map (\x -> List.length x >= 1) |>
                                                  Maybe.withDefault False
                                in
                                    case (isEmpty, hasSiblings) of
                                        (False, _) -> R.fail "Cannot delete a non-empty terminal"
                                        (True, False) -> R.fail "Cannot delete an only child"
                                        (True, True) -> Tree.extractAt path (.get Model.root model) |>
                                                        Maybe.map Tuple.second |>
                                                        R.liftVal "deleteTerminal"

                            children ->
                                let
                                    newRoot = Tree.extractAt path root |> Maybe.map Tuple.second
                                in
                                    Maybe.andThen (Tree.insertManyAt path children) newRoot |>
                                    R.liftVal "deleteNonTerminal"
    in
        R.succeed model |>
        Selection.withOne model.selected (delete >> R.map (flip (.set Model.root) model)) |>
        R.andThen clearSelection


editLabel : Model -> Result
editLabel model =
    let
        selected : Maybe Path.Path
        selected = model |> .get Model.selected |> Selection.getOne
        root : Maybe Tree
        root = model |> .get Model.root |> Just
        label : R.Result String
        label = maybeAndThen2 Tree.get selected root |> Maybe.map (.get Tree.label) |> R.liftVal "editLabel"
        initForm = R.map LabelEdit.init label
    in
        R.map (\l -> { model | labelForm = Just <| LabelEdit.init l}) label |>
        R.do (Dom.focus "labelEditor" |> Task.onError (always <| Task.succeed ()) |> Task.perform (always Msg.Ignore))

finishLabelEdit : Model -> Result
finishLabelEdit model =
    let
        selected = model |> .get Model.selected |> Selection.first |> R.liftVal "nothing selected"
        root = model |> .get Model.root
        newLabel = model.labelForm |> R.liftVal "not editing" |> R.andThen LabelEdit.finish
        changeLabel = newLabel |> R.map (.set Tree.label)
    in
        R.andThen3 doAt selected changeLabel (R.succeed model) |>
        R.map (\m -> { m | labelForm = Nothing })

toggleDashTag : String -> Path -> Model -> Result
toggleDashTag tag path model =
    let
        tree = model |> .get Model.root |> Tree.get path
        labels = tree |> Maybe.map (.get Tree.label >> String.split "-")
        contains = labels |> Maybe.map (List.any ((==) tag))
        setLabel = .set Tree.label
        valuePre =
            case contains of
                Just True -> (labels |> Maybe.map (List.filter ((/=) tag) >> String.join "-"))
                Just False -> (labels |> Maybe.map ((\x -> x ++ [tag]) >> String.join "-"))
                Nothing -> Nothing
    in
        valuePre |>
        R.liftVal "toggleDashTag" |>
        R.andThen (\x -> doAt path (setLabel x) model)

undo : Model -> Result
undo _ = R.fail "bogus message" |> R.do (Utils.cmd Undo)

redo : Model -> Result
redo _ = R.fail "bogus message" |> R.do (Utils.cmd Redo)
