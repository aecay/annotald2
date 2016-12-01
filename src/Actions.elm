module Actions exposing ( clearSelection
                        , changeLabel
                        , coIndex
                        , Action
                        , liftMaybe, Failure(..), Result -- TODO: don't want to export
                        )

import Maybe exposing (withDefault)
import Maybe.Extra
--import List.Extra exposing (singleton)
--import List exposing (head, tail)

import Tree exposing (Path, Tree)

import Utils exposing ((?>), (?>?), zip)

import Dict exposing (Dict)

import TreeExts as TX
import MultiwayTree as MT

import Model exposing (Model, refresh)
import Selection
import Result as R


type Failure = Silent | Msg String

type alias Result = R.Result Failure Model

type alias Action = Model -> Result

liftMaybe : Failure -> Maybe a -> R.Result Failure a
liftMaybe f m =
    case m of
        Just m -> R.Ok m
        Nothing -> R.Err f

flattenMaybe : Failure -> R.Result Failure (Maybe a) -> R.Result Failure a
flattenMaybe failure resultMaybe =
    case resultMaybe of
        R.Ok x -> case x of
                      Just y -> succeed y
                      Nothing -> R.Err failure
        R.Err e -> R.Err e

succeed : a -> R.Result Failure a
succeed = R.Ok

fail : Failure -> Result
fail = R.Err

fmap : (a -> b) -> R.Result Failure a -> R.Result Failure b
fmap f result =
    case result of
        R.Ok m -> R.Ok (f m)
        R.Err e -> R.Err e

bind : (a -> R.Result Failure b) -> R.Result Failure a -> R.Result Failure b
bind f result =
    case result of
        R.Ok a -> f a
        R.Err e -> R.Err e

doOneSelected : (Tree -> Tree) -> Model -> Result
doOneSelected f model =
    let
        path = model.selected |>
               Selection.first
    in liftMaybe Silent path |>
    bind (\x -> doAt x f model)

doAt : Path -> (Tree -> Tree) -> Model -> Result
doAt path f model =
    let
        root = model.root
    in
        root |>
        Tree.get path |>
        liftMaybe Silent |>
        fmap f |>
        fmap (\x -> Tree.set path x root) |>
        flattenMaybe Silent |>
        fmap (refresh model)

clearSelection : Action
clearSelection m = succeed { m | selected = Selection.empty }

changeLabel : List String -> Action
changeLabel labels =
    case labels of
        [] -> succeed
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

-- TODO: with one node selected, remove index
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
            otherwise -> fail Silent

coIndex2 : Path -> Path -> Model -> Result
coIndex2 f s model =
    case Tree.root f == Tree.root s of
        False -> fail (Msg "Can't coindex nodes in two different roots")
        True ->
            let
                root = model.root
                ft = Tree.get f root
                st = Tree.get s root
                fi = ft ?> MT.datum ?> .index |> Maybe.Extra.join ?> .number
                si = st ?> MT.datum ?> .index |> Maybe.Extra.join ?> .number
                ind = Tree.get (Tree.root f) root ?>
                      Tree.highestIndex |>
                      Maybe.withDefault 0 |>
                      (+) 1
            in
                case (fi, si) of
                    -- One of the nodes has an index, the other does not: set
                    -- the index of the unindexed node to match
                    (Nothing, Just x) -> setIndexAt f x model
                    (Just x, Nothing) -> setIndexAt s x model
                    -- Both of the nodes have an index: remove both indices
                    -- (TODO: toggle index types)
                    (Just x, Just y) -> removeIndexAt f model |> bind (removeIndexAt s)
                    (Nothing, Nothing) ->  setIndexAt f ind model |> bind (setIndexAt s ind)

setIndexAt: Path -> Int -> Action
setIndexAt path index =
    let
        f x = { x | index = Just { number = index, variety = Tree.Normal } }
    in
        doAt path (TX.updateDatum f)

removeIndexAt: Path -> Action
removeIndexAt path =
    let
        f x = { x | index = Nothing }
    in
        doAt path (TX.updateDatum f)
