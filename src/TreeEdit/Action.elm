module TreeEdit.Action exposing
    ( Action
    , actions
    , doMove
    , doMoveToRoot
    , finishLabelEdit
    , createLeafBefore -- For ContextMenu
    , createLeafAfter  -- diito
    , toggleDashTag    -- ditto
    , move2
    )

import Array exposing (Array)
import Browser.Dom
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Task

import List.Extra exposing (zip)
import Monocle.Common exposing (maybe)
import Monocle.Lens as Lens
import Monocle.Optional as Optional exposing (fromLens)

import TreeEdit.Index as Index exposing (Variety(..), normal)
import TreeEdit.Model as Model
import TreeEdit.Model.Type exposing (ForestModel)
import TreeEdit.Msg as Msg exposing (Msg(..))
import TreeEdit.OrderedDict as OD
import TreeEdit.Path as Path exposing (Path(..))
import TreeEdit.Result as R exposing (Result(..))
import TreeEdit.Selection as Selection
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type as TreeType exposing (Forest, Tree, constants)
import TreeEdit.Utils as Utils exposing (maybeAndThen2, o, and, andO, fromJust, indexOf)
import TreeEdit.View.LabelEdit as LabelEdit


type alias Result = R.Result ForestModel

type alias Action = ForestModel -> Result


actions :
    { changeLabel : List String -> Action
    , clearSelection : Action
    , coIndex : Action
    , createParent : String -> Action
    , deleteNode : Action
    , editLabel : Action
    , leafAfter : Tree -> Action
    , leafBefore : Tree -> Action
    }
actions =
    { clearSelection = clearSelection
    , coIndex = coIndex
    , editLabel = editLabel
    , changeLabel = changeLabel
    , leafAfter = leafAfter
    , createParent = createParent
    , leafBefore = leafBefore
    , deleteNode = deleteNode
    }


-- TODO: remove this fn
doAt : Path -> (Tree -> Tree) -> ForestModel -> Result
doAt path f model =
    R.succeed { model | root = Lens.modify (Tree.path path) f model.root}


clearSelection : Action
clearSelection m = R.succeed { m | selected = Selection.empty }


changeLabel : List String -> ForestModel -> Result
changeLabel labels model =
    let
        selected = model.selected |> Selection.getOne
    in
    case selected of
        Just sel ->
            if
                Tree.get sel model.root
                    |> Tree.hasTerminalLabel
            then
                R.fail "leaf selected"

            else
                case labels of
                    [] ->
                        R.succeed model

                    head :: tail ->
                        let
                            pairs : List ( String, String )
                            pairs = tail ++ [ head ] |> zip (head :: tail)

                            repls : Dict String String
                            repls = Dict.fromList pairs

                            change : String -> String
                            change s = Dict.get s repls |> withDefault head

                            update : Tree -> Tree
                            update = Lens.modify Tree.label change
                        in
                            doAt sel update model

        Nothing ->
            R.fail "nothing or two things selected"


coIndex : ForestModel -> Result
coIndex model =
    Selection.perform model.selected
        (R.fail "nothing selected")
        (coIndex1 model >> R.succeed)
        (coIndex2 model)


coIndex1 : ForestModel -> Path -> ForestModel
coIndex1 b a = removeIndexAt a b


coIndex2 : ForestModel -> Path -> Path -> Result
coIndex2 model path1 path2 =
    case Path.root path1 == Path.root path2 of
        False -> R.failWarn "Can't coindex nodes in two different roots"

        True ->
            let
                root = model.root
                tree1 = Tree.get path1 root
                tree2 = Tree.get path2 root
                index1 = tree1 |> .get Tree.index
                index2 = tree2 |> .get Tree.index
                ind =
                    Tree.get (Path.root path1) root
                        |> Tree.highestIndex
                        |> -- TODO: I think this
                           -- logic is duplicated
                           -- elsewhere in this file
                           (+) 1

                helper : Maybe Index.Index -> Maybe Index.Index -> Result
                helper i1 i2 =
                    R.succeed <|
                        case ( i1, i2 ) of
                            -- One of the nodes has an index, the other does not: set
                            -- the index of the unindexed node to match
                            ( Nothing, Just x ) ->
                                setIndexAt path1 x.number model

                            ( Just x, Nothing ) ->
                                setIndexAt path2 x.number model

                            -- Both of the nodes have an index: toggle index type
                            -- TODO: must check that indices are both equal
                            ( Just x, Just y ) ->
                                case ( x.variety, y.variety ) of
                                    -- Normal coindexing -> gap
                                    ( Index.Normal, Index.Normal ) ->
                                        setIndexVarietyAt path2 Index.Gap model
                                            |> (\xx ->
                                                    if isGapAt path2 xx
                                                    then xx
                                                    else setIndexVarietyAt path1 Index.Gap model
                                               )

                                    -- Gap -> backwards gap
                                    ( Index.Normal, Index.Gap ) ->
                                        setIndexVarietyAt path1 Index.Gap model
                                            |> setIndexVarietyAt path2 Index.Normal

                                    -- Backwards gap -> remove indexes
                                    ( Index.Gap, Index.Normal ) ->
                                        removeIndexAt path1 model
                                            |> removeIndexAt path2

                                    -- Something weird -> remove indexes (TODO: is
                                    -- this right?)
                                    otherwise ->
                                        removeIndexAt path1 model
                                            |> removeIndexAt path2

                            -- Neither node has an index -> coindex them
                            ( Nothing, Nothing ) ->
                                setIndexAt path1 ind model
                                    |> setIndexAt path2 ind
            in
            helper index1 index2


setIndexAt : Path -> Int -> ForestModel -> ForestModel
setIndexAt path index model =
    { model | root = .set (Tree.path path |> and Tree.index) (Just <| Index.normal index) model.root }



setIndexVarietyAt : Path -> Index.Variety -> ForestModel -> ForestModel
setIndexVarietyAt path newVariety model =
    let
        lens =
            o (Tree.path path |> and Tree.index) |> andO maybe |> andO (o Index.variety)
    in
        { model | root = .set lens newVariety model.root }



isGapAt : Path -> ForestModel -> Bool
isGapAt path model =
    let
        lens = (Tree.path path |> and Tree.index |> o) |> andO maybe |> andO (o Index.variety)
    in
    .getOption lens model.root
      |> Maybe.withDefault Index.Normal
      |> (==) Index.Gap


removeIndexAt : Path -> ForestModel -> ForestModel
removeIndexAt path model =
    { model | root = .set (Tree.path path |> and Tree.index) Nothing model.root }


incrementIndicesBy : Int -> Path -> Forest -> Forest
incrementIndicesBy inc path trees =
    Optional.modify (o Tree.index |> andO maybe |> andO (o Index.number)) ((+) inc)
        |> Tree.map
        |> (\x -> Lens.modify (Tree.path path) x trees)



-- TODO: use Id type instead of String


newRootTree : Int -> ForestModel -> Tree -> (ForestModel, Path)
newRootTree index model tree =
    let
        ( newModel, newId ) = Model.freshUuid model
        newTree = tree |> Lens.modify Tree.metadata (Dict.update "ID" (\_ -> Just newId))
        newModel2 = { model | root = (OD.insertAt index newId newTree newModel.root) }
    in
        (newModel2, Path.singleton newId)


doMoveToRoot : Path -> ForestModel -> Result
doMoveToRoot src model =
    let
        forest = model.root
        (Path id frag) = src
        root = Path.singleton id
        first = Tree.allFirst root frag forest
        last = Tree.allLast root frag forest
        index = OD.elemIndex id forest |> Utils.fromJust
        go i =
            let
                tree = Tree.get src forest
                (newModel, newSel) = newRootTree i model tree
            in
                { newModel |
                  root = Tree.deleteAt src newModel.root
                , selected = Selection.one newSel
                }
    in
    if first then
        go index |> R.succeed
    else if last then
        go (index + 1) |> R.succeed
    else
        R.fail "cannot move to root from middle"


doMove : Path -> Path -> ForestModel -> Result
doMove src dest model =
    let
        srcRoot = Path.root src
        destRoot = Path.root dest
        rootTree = model.root
        -- If we are moving withing the same root tree, then do nothing
        -- special.  If we are moving a tree up to the root level, then give
        -- it an ID.  If we are moving a tree from one root tree into another,
        -- increment the indices in the moved tree so that there is no clash.
        newRoot =
            if srcRoot == destRoot then
                rootTree
            else
                let
                    inc =
                        Tree.get destRoot rootTree
                            |> Tree.highestIndex
                in
                incrementIndicesBy inc srcRoot rootTree

        res = Tree.moveTo src dest newRoot
    in
        R.map (\(nr, sel) -> { model | root = nr, selected = Selection.one sel }) res


createParent2 : String -> ForestModel -> Path -> Path -> Result
createParent2 label model one two =
    let
        (Path id1 frag1) = one
        (Path id2 frag2) = two
    in
    if id1 /= id2 || List.tail frag1 /= List.tail frag2 then
        R.fail "parents are different for createParent2"
    else
        case ( frag1, frag2 ) of
            ( [], [] ) ->
                -- TODO
                Debug.log "unimplemented" () |> always (R.succeed model)

            ( foot1_ :: rest1, foot2_ :: rest2 ) ->
                let
                    ( foot1, foot2 ) = Utils.sort2 foot1_ foot2_
                    parentPath = (Path id1 rest1)
                in
                doAt parentPath
                    (Lens.modify Tree.children
                        (\c ->
                            let
                                ( x, y, z ) =
                                    Utils.splice foot1 (foot2 + 1) c
                            in
                            Array.append x <|
                                Array.append
                                    (Array.repeat 1
                                        (constants.nonterminal y { label = label
                                                                 , metadata = Dict.empty
                                                                 , index = Nothing
                                                                 }
                                        )
                                    )
                                    z
                        )
                    )
                    { model | selected = Selection.one <| Path.childPath foot1 parentPath }

            _ ->
                -- Guaranteed by parent1 /= parent2 check above
                R.fail "impossible case in createParent2"


createParent : String -> ForestModel -> Result
createParent label model =
    let
        one : Path -> Result
        one path =
            doAt path (\x -> .ta TreeType.private label <| Array.repeat 1 x) model
    in
    Selection.perform model.selected (R.fail "nothing selected") one (createParent2 label model)


doMovement : (Tree -> ForestModel -> Path -> Result) -> ForestModel -> Path -> Path -> Result
doMovement insertLeaf model dest src =
    let
        root = model.root
        indDef =
            root
                |> Tree.get (Path.root src)
                |> Tree.highestIndex
                |> (+) 1
        ind =
            root
                |> Tree.get src
                |> .get Tree.index
                |> Maybe.map (.get Index.number)
                |> Maybe.withDefault indDef

        -- TODO: should use lens functions and not let this be a Result, then
        -- we can eliminate the andThen3 below
        m = doAt src (.set Tree.index <| Just <| Index.normal ind) model

        -- TODO: can get the trace from the passed in node, no need to
        -- separately pass the index
        trace : R.Result Tree
        trace =
            m
              |> R.map .root
              |> R.andThen (Tree.get src >> R.succeed)
              |> R.map (\a -> Tree.makeTrace a ind)

        sameRoot = Path.root src == Path.root dest
    in
    if sameRoot then
        R.andThen3 insertLeaf trace m (R.succeed dest)
          |> R.map (\x -> {x | selected = Selection.one dest})

    else
        R.fail "Can't make movement trace across different root nodes"


createLeaf_ : (Int -> Int) -> Tree -> ForestModel -> Path -> Result
createLeaf_ fn leaf m path =
    let
        forest = m.root
        (Path id frag) = path
    in
        case frag of
            [] ->
                let
                    index = OD.elemIndex id forest |> Utils.fromJust
                in
                    newRootTree (fn index) m leaf |> Tuple.first |> R.succeed
            foot :: rest ->
                let
                    parent = Path id rest
                    update c = Utils.insert (fn foot) leaf c
                in
                    Lens.modify (Tree.path parent |> and Tree.children) update forest
                      |> (\x -> {m | root = x})
                      |> R.succeed

createLeafBefore : Tree -> ForestModel -> Path -> Result
createLeafBefore = createLeaf_ identity

createLeafAfter : Tree -> ForestModel -> Path -> Result
createLeafAfter = createLeaf_ (\x -> x + 1)


leafBefore : Tree -> ForestModel -> Result
leafBefore newLeaf model =
    Selection.perform model.selected
        (R.fail "nothing selected")
        (createLeafBefore newLeaf model)
        (doMovement createLeafBefore model)


leafAfter : Tree -> ForestModel -> Result
leafAfter newLeaf model =
    Selection.perform model.selected
        (R.fail "nothing selected")
        (createLeafAfter newLeaf model)
        (doMovement createLeafAfter model)


deleteNode : Action
deleteNode model =
    case Selection.getOne model.selected of
        Nothing -> R.fail "nothing selected"
        Just sel ->
            case Path.parent sel of
                Nothing ->
                    let
                        forest = model.root
                        tree = Tree.get sel forest
                        -- TODO: eliminate this use of fromJust by making a
                        -- RootTree type that obligatorily has an ID
                        id = tree |> .get Tree.metadata |> Dict.get "ID" |> fromJust
                        idx = indexOf id <| OD.keys forest
                        children = tree |> .get Tree.children
                        add : Tree -> ForestModel -> ForestModel
                        add t m = newRootTree idx m t |> Tuple.first
                        newForest = OD.remove id forest
                    in
                        { model | root = newForest }
                          |> (\x -> Array.foldr add x children)
                          |> clearSelection
                Just parent ->
                    let
                        forest = model.root
                        n = Tree.get sel forest
                        children = .get Tree.children n
                    in
                    case children |> Array.length of
                        0 ->
                            let
                                isEmpty = Tree.isEmpty n

                                hasSiblings =
                                    parent
                                      |> (\a -> Tree.get a forest)
                                      |> .get Tree.children
                                      |> (\x -> Array.length x >= 1)
                            in
                            if not isEmpty then
                                R.fail "Cannot delete a non-empty terminal"
                            else if not hasSiblings then
                                R.fail "Cannot delete an only child"
                            else
                                {model | root = Tree.deleteAt sel forest }
                                  |> clearSelection
                        _ ->
                            Tree.deleteAt sel forest
                              |> Tree.insertManyAt sel children
                              |> (\x -> { model | root = x })
                              |> clearSelection

editLabel : ForestModel -> Result
editLabel model =
    let
        selected : Maybe Path.Path
        selected = model.selected |> Selection.getOne
        root = model.root
        label : R.Result String
        label =
            Maybe.map (\x -> Tree.get x root) selected |> Maybe.map (.get Tree.label) |> R.liftVal "editLabel"
    in
    R.map (\l -> { model | labelForm = Just <| LabelEdit.init l }) label
        |> R.do (Browser.Dom.focus "labelEditor" |> Task.onError (always <| Task.succeed ()) |> Task.perform (always Msg.Ignore))


finishLabelEdit : ForestModel -> Result
finishLabelEdit model =
    let
        selected = model.selected |> Selection.first |> R.liftVal "nothing selected"
        root = model.root
        newLabel =
            model.labelForm |> R.liftVal "not editing" |> R.andThen LabelEdit.finish
        changeLbl = newLabel |> R.map (.set Tree.label)
    in
    R.andThen3 doAt selected changeLbl (R.succeed model)
      |> R.map (\m -> { m | labelForm = Nothing })


toggleDashTag : String -> Path -> ForestModel -> Result
toggleDashTag tag path model =
    let
        tree = model.root |> Tree.get path
        labels = tree |> .get Tree.label |> String.split "-"
        contains = labels |> List.any ((==) tag)
        setLabel = .set Tree.label
        valuePre =
            case contains of
                True ->
                    labels |> List.filter ((/=) tag) |> String.join "-"

                False ->
                    labels |> (\x -> x ++ [ tag ]) |> String.join "-"
    in
    valuePre
      |> R.succeed
      |> R.andThen (\x -> doAt path (setLabel x) model)

move2 : Path -> Path -> Path -> ForestModel -> Result
move2 dest first second model =
    let
        (Path id1 frag1) = first
        (Path id2 frag2) = second
    in
        if id1 /= id2 then
            R.fail "unimplemented"
        else
            if List.tail frag1 /= List.tail frag2 then
                R.fail "can't move multiple nodes that are daughters of different parents"
            else
                createParent2 "XP" model first second
                    -- TODO: this is a sort of hacky way of not having to
                    -- implement path sorting code to decide which of first or
                    -- second to pass as the argument to doMove...
                  |> R.andThen (\x -> Selection.withOne x.selected (\y -> doMove y dest x) (R.succeed x))
                  |> R.andThen deleteNode
