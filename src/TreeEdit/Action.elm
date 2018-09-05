module TreeEdit.Action exposing ( Action
                                , actions
                                , doMove
                                , doMoveToRoot
                                , leafBeforeInner -- For ContextMenu TODO:
                                                  -- consolidate with createLeaf
                                , finishLabelEdit
                                , toggleDashTag
                                )

-- This module contains the types and functions for creating *actions*, or
-- functions that respond to user input.

-- Standard library

import Array exposing (Array)
import Dict exposing (Dict)
import Dom
import Maybe exposing (withDefault)
import Task

-- Third party

import List.Extra exposing (zip)
import Monocle.Common exposing ((<|>), (=>), maybe)
import Monocle.Lens as Lens
import Monocle.Optional as Optional exposing (fromLens)

-- Annotald packages

import TreeEdit.Index as Index exposing (normal, Variety(..))
import TreeEdit.Model as Model
import TreeEdit.Model.Type exposing (Model)
import TreeEdit.Msg as Msg
import TreeEdit.Msg exposing (Msg(Undo, Redo))
import TreeEdit.OrderedDict as OD
import TreeEdit.Path as Path exposing (Path)
import TreeEdit.Result as R exposing (Result(..))
import TreeEdit.Selection as Selection
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type as TreeType exposing (Tree, Forest)
import TreeEdit.Utils as Utils exposing (maybeAndThen2, o)
import TreeEdit.View.LabelEdit as LabelEdit

type alias Result = R.Result Model

-- The rest of the file

type alias Action = Model -> Result

actions :
    { changeLabel : List String -> Action
    , clearSelection : Action
    , coIndex : Action
    , createParent : String -> Action
    , deleteNode : Action
    , editLabel : Action
    , leafAfter : Tree -> Action
    , leafBefore : Tree -> Action
    , redo : Action
    , undo : Action
    }
actions =
    { clearSelection = clearSelection
    , coIndex = coIndex
    , editLabel = editLabel
    , undo = undo
    , redo = redo
    , changeLabel = changeLabel
    , leafAfter = leafAfter
    , createParent = createParent
    , leafBefore = leafBefore
    , deleteNode = deleteNode
    }

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

incrementIndicesBy : Int -> Path -> Forest -> Forest
incrementIndicesBy inc path trees =
    Optional.modify ((o Tree.index) => maybe => (o Index.number)) ((+) inc) |>
    Tree.map |>
    (\x -> Lens.modify (Tree.path path) x trees)

-- TODO: use Id type instead of String
newRootTree : Int -> Model -> (Model, String)
newRootTree index model =
    let
        (newModel, newId) = Model.freshUuid model
        newTree = TreeType.root newId
        newModel2 = Lens.modify Model.root (OD.insertAt index newId newTree) newModel
    in
        (newModel2, newId)

doMoveToRoot : Path -> Model -> Result
doMoveToRoot src model =
    let
        forest = .get Model.root model
        root = Path.root src
        frag = Path.subtract root src |> Utils.fromJust
        first = Tree.allFirst root frag forest
        last = Tree.allLast root frag forest
        (id, _) = Path.decompose root
        index = OD.elemIndex id forest |> Utils.fromJust
        go i =
            let
                (newModel, newId) = newRootTree i model
            in
                doMove src (Path.singleton newId) newModel
    in
        if first
        then go index
        else
            if last
            then go <| index + 1
            else R.fail "cannot move to root from middle"

doMove : Path -> Path -> Model -> Result
doMove src dest model =
    let
        srcRoot = Path.root src
        destRoot = Path.root dest
        rootTree = .get Model.root model
        -- If we are moving withing the same root tree, then do nothing
        -- special.  If we are moving a tree up to the root level, then give
        -- it an ID.  If we are moving a tree from one root tree into another,
        -- increment the indices in the moved tree so that there is no clash.
        newRoot = if srcRoot == destRoot
                  then rootTree
                  else
                      let
                          inc = (Tree.get destRoot rootTree) |>
                                Tree.highestIndex
                      in
                          incrementIndicesBy inc srcRoot rootTree
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
    in
        if parent1 /= parent2
        then R.fail "parents are different for createParent2"
        else
            case (parent1, parent2) of
                (Nothing, Nothing) -> Debug.crash "unimplemented" -- TODO
                (Just p1, Just p2) ->
                    let
                        (foot1, foot2) = Utils.sort2
                                         (Path.foot one |> Utils.fromJust)
                                         (Path.foot two |> Utils.fromJust)
                    in
                        doAt p1 (Lens.modify Tree.children
                                     (\c ->
                                          let
                                              (x, y, z) = Utils.splice foot1 (foot2+1) c
                                          in
                                              Array.append x <|
                                              Array.append (Array.repeat 1
                                                                (.ta TreeType.private label y)) z))
                            model |> R.map ((.set Model.selected) (Selection.one <| Path.childPath foot1 p1))
                _ -> Debug.crash "impossible" -- Guaranteed by parent1 /= parent2 check above

createParent : String -> Model -> Result
createParent label model =
    let
        one : Path -> Result
        one path = doAt path (\x -> .ta TreeType.private label <| Array.repeat 1 x) model
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

leafBeforeInner : Tree -> Path -> Forest -> R.Result Forest
leafBeforeInner newLeaf path trees =
    let
        parent = Path.parent path |> Utils.fromJust
        foot = Path.foot path |> Utils.fromJust
        update c = Utils.insert foot newLeaf c
    in
        Lens.modify ((Tree.path parent) <|> Tree.children) update trees |>
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


deleteNode : Action
deleteNode model =
    let
        root = .get Model.root model
        delete : Path -> R.Result Forest
        delete path =
            let
                parent_ = Path.parent path
            in
                case parent_ of
                    Nothing -> Debug.crash "TODO: not implemented"
                    -- TODO: add Ids to new children, insert in root in proper place
                    Just parent ->
                        let
                            n = Tree.get path root
                            children = .get Tree.children n
                        in
                            case children |> Array.length of
                                0 ->
                                    let
                                        isEmpty = Tree.isEmpty n
                                        hasSiblings = parent |>
                                                      flip Tree.get root |>
                                                      .get Tree.children |>
                                                      (\x -> Array.length x >= 1)
                                    in
                                        case (isEmpty, hasSiblings) of
                                            (False, _) -> R.fail "Cannot delete a non-empty terminal"
                                            (True, False) -> R.fail "Cannot delete an only child"
                                            (True, True) -> R.succeed <| Tree.deleteAt path (.get Model.root model)

                                _ ->
                                    Tree.deleteAt path root |>
                                    Tree.insertManyAt path children |>
                                    R.succeed
    in
        R.succeed model |> -- TODO: why this succeed?
        Selection.withOne model.selected (delete >> R.map (flip (.set Model.root) model)) |>
        R.andThen clearSelection


editLabel : Model -> Result
editLabel model =
    let
        selected : Maybe Path.Path
        selected = model |> .get Model.selected |> Selection.getOne
        root = model |> .get Model.root
        label : R.Result String
        label = Maybe.map (\x -> Tree.get x root) selected |> Maybe.map (.get Tree.label) |> R.liftVal "editLabel"
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
