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

import Array exposing (Array)
import Dom
import Maybe exposing (withDefault)
import Maybe.Extra
import Dict exposing (Dict)
import TreeEdit.Result as R exposing (Result(..))
import Task
import Monocle.Common exposing ((<|>))
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

doAt : Path -> (Tree -> Tree) -> Model -> Result
doAt path f model =
    Lens.modify (Model.root <|> (Tree.path path)) f model |>
    R.succeed

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
                    |> Tree.hasTerminalLabel
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
        (R.fail "nothing selected")
        (coIndex1 model >> R.succeed)
        (coIndex2 model)

coIndex1: Model -> Path -> Model
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
                index1 = tree1 |> .get Tree.index
                index2 = tree2 |> .get Tree.index
                ind = Tree.get (Path.root path1) root |>
                      Tree.highestIndex |> -- TODO: I think this
                                           -- logic is duplicated
                                           -- elsewhere in this file
                      (+) 1
                helper : Maybe Index.Index -> Maybe Index.Index -> Result
                helper i1 i2 =
                    R.succeed <|
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
                                    (\x -> if isGapAt path2 x
                                           then x
                                           else setIndexVarietyAt path1 Index.Gap model)
                                -- Gap -> backwards gap
                                (Index.Normal, Index.Gap) ->
                                    setIndexVarietyAt path1 Index.Gap model |>
                                    setIndexVarietyAt path2 Index.Normal
                                -- Backwards gap -> remove indexes
                                (Index.Gap, Index.Normal) ->
                                    removeIndexAt path1 model |>
                                    removeIndexAt path2
                                -- Something weird -> remove indexes (TODO: is
                                -- this right?)
                                otherwise -> removeIndexAt path1 model |>
                                             removeIndexAt path2
                        -- Neither node has an index -> coindex them
                        (Nothing, Nothing) -> setIndexAt path1 ind model |>
                                              setIndexAt path2 ind
            in
                helper index1 index2

setIndexAt: Path -> Int -> Model -> Model
setIndexAt path index =
    .set (Model.root <|> (Tree.path path) <|> Tree.index) (Just <| Index.normal index)

setIndexVarietyAt : Path -> Index.Variety -> Model -> Model
setIndexVarietyAt path newVariety =
    let
        lens = (o (Model.root <|> (Tree.path path) <|> Tree.index)) => maybe => (o Index.variety)
    in
        .set lens newVariety

isGapAt : Path -> Model -> Bool
isGapAt path model =
    let
        lens = (o <| Model.root <|> (Tree.path path) <|> Tree.index) => maybe => (o Index.variety)
    in
        .getOption lens model |>
        Maybe.withDefault Index.Normal |>
        (==) Index.Gap

removeIndexAt : Path -> Model -> Model
removeIndexAt path =
    .set (Model.root <|> (Tree.path path) <|> Tree.index) Nothing

incrementIndicesBy : Int -> Path -> Tree -> Tree
incrementIndicesBy inc path tree =
    Optional.modify ((o Tree.index) => maybe => (o Index.number)) ((+) inc) |>
    Tree.map |>
    (\x -> Lens.modify (Tree.path path) x tree)

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
                                Tree.highestIndex
                      in
                          incrementIndicesBy inc srcRoot (.get Model.root model)
        res = Tree.moveTo src dest newRoot
        newRoot1 = R.map Tuple.first res
        newSel = R.map Tuple.second res
    in
        R.modify Model.root (always newRoot1) model |>
        R.andThen (R.modify Model.selected (always <| R.map Selection.one newSel))

createParent2 : String -> Model -> Path -> Path -> Result
createParent2 label model one two =
    let
        parent1 = Path.parent one
        parent2 = Path.parent two
        (foot1, foot2) = Utils.sort2 (Path.foot one) (Path.foot two)
    in
        if parent1 /= parent2
        then R.fail "parents are different for createParent2"
        else doAt parent1 (Lens.modify Tree.children (\c ->
                                                          let
                                                              (x, y, z) = Utils.splice foot1 (foot2+1) c
                                                            in
                                                                Array.append x <|
                                                                Array.append (Array.repeat 1
                                                                                  (.t TreeType.private label y))
                                                                z))
            model |> R.map ((.set Model.selected) (Selection.one <| Path.childPath foot1 parent1))

createParent : String -> Model -> Result
createParent label model =
    let
        one : Path -> Result
        one path = doAt path (\x -> .t TreeType.private label <| Array.repeat 1 x) model
    in
        Selection.perform model.selected (R.fail "nothing selected") one (createParent2 label model)

doMovement : Model -> Path -> Path -> Result
doMovement model dest src =
    let
        root = (.get Model.root) model
        indDef = root |>
                 Tree.get (Path.root src) |>
                 Tree.highestIndex |>
                 (+) 1
        ind = root |>
              Tree.get src |>
              .get Tree.index |>
              Maybe.map (.get Index.number) |>
              Maybe.withDefault indDef
        m = doAt src (.set Tree.index <| Just <| Index.normal ind) model
        -- TODO: can get the trace from the passed in node, no need to
        -- separately pass the index
        trace : R.Result Tree
        trace = m |>
                R.map (.get Model.root) |>
                R.andThen (Tree.get src >> R.succeed) |>
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
        update c = Utils.insert foot newLeaf c
    in
        Lens.modify ((Tree.path parent) <|> Tree.children) update tree |>
        R.succeed -- TODO: bogus succeed

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
                n = Tree.get path root
                children = .get Tree.children n
            in
                case children |> Array.length of
                    0 ->
                        let
                            isEmpty = Tree.isEmpty n
                            hasSiblings = path |>
                                          Path.parent |>
                                          flip Tree.get root |>
                                          .get Tree.children |>
                                          (\x -> Array.length x >= 1)
                        in
                            case (isEmpty, hasSiblings) of
                                (False, _) -> R.fail "Cannot delete a non-empty terminal"
                                (True, False) -> R.fail "Cannot delete an only child"
                                (True, True) -> Tree.extractAt path (.get Model.root model) |>
                                                Tuple.second |>
                                                R.succeed

                    _ ->
                        let
                            newRoot = Tree.extractAt path root |> Tuple.second
                        in
                            Tree.insertManyAt path children newRoot |>
                            R.succeed
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
        label = Maybe.map Tree.get selected |> Maybe.Extra.andMap root |> Maybe.map (.get Tree.label) |> R.liftVal "editLabel"
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
        labels = tree |> .get Tree.label |> String.split "-"
        contains = labels |> List.any ((==) tag)
        setLabel = .set Tree.label
        valuePre =
            case contains of
                True -> labels |> List.filter ((/=) tag) |> String.join "-"
                False -> labels |> (\x -> x ++ [tag]) |> String.join "-"
    in
        valuePre |>
        R.succeed |>
        R.andThen (\x -> doAt path (setLabel x) model)

undo : Model -> Result
undo _ = R.fail "bogus message" |> R.do (Utils.cmd Undo)

redo : Model -> Result
redo _ = R.fail "bogus message" |> R.do (Utils.cmd Redo)
