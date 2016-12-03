module Actions exposing ( clearSelection
                        , changeLabel
                        , coIndex
                        , Action
                        , liftMaybe, Failure(..), Result -- TODO: don't want to export
                        )

{-| This module contains the types and functions for creating *actions*, or
functions that respond to user input.

# The Result type

@docs Failure, Result, Action

## Operations on Results

@docs succeed, fail, fmap, bind, liftMaybe, flattenMaybe

# TODO

@docs changeLabel, clearSelection, coIndex

-}

-- Standard library

import Maybe exposing (withDefault)
import Dict exposing (Dict)
import Result as R

-- Third party

import Maybe.Extra
import MultiwayTree as MT

-- Annotald packages

import Tree exposing (Path, Tree)
import Utils exposing ((?>), (?>?), zip)
import TreeExts as TX
import Model exposing (Model, refresh)
import Selection

-- Result types and functions

{-| This type represents failures during the execution of a user command.

The `Silent` failure type comes with a message, which is counterintuitive but
useful for debugging.  (Unlike for the `Msg` type, `Silent` failures are not
displayed to the user.)

-}
type Failure = Silent String | Msg String

{-| This type represents the result of applying a user command.

The command can succeed with a new model (which replaces the old one), or fail
with a `Failure`.
-}
type alias Result = R.Result Failure Model

{-| Other sections of Annotald are written as functions operating on trees,
not in a user interaction context.  These signal success or failure with
`Maybe`.  This function "lifts" those values into the `Result` context, making
them suitable for use in user interaction functions.
-}
liftMaybe : Failure -> Maybe a -> R.Result Failure a
liftMaybe f m =
    case m of
        Just m -> R.Ok m
        Nothing -> R.Err f

{-| A similar problem occurs when we already have a `Result` and wish to apply
one of the non-user functions to it, but we get the `Maybe` inside the
`Result` (rather than instead of it, as in the case where we want to
`liftMaybe`).  This function takes a `Maybe` embedded inside a `Result` and
turns it into just a result.
-}
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

{-| Fmap for `Result`.

See also the [Functor typeclass](https://wiki.haskell.org/Typeclassopedia#Functor).
-}
fmap : (a -> b) -> R.Result Failure a -> R.Result Failure b
fmap f result =
    case result of
        R.Ok m -> R.Ok (f m)
        R.Err e -> R.Err e

{-| Monadic bind for `Result`.

See also the [Monad
typeclass](https://wiki.haskell.org/Typeclassopedia#Monad).  (`return` is
given by `succeed`.)
-}
bind : (a -> R.Result Failure b) -> R.Result Failure a -> R.Result Failure b
bind f result =
    case result of
        R.Ok a -> f a
        R.Err e -> R.Err e

-- The rest of the file

{-| TODO
-}
type alias Action = Model -> Result

doOneSelected : (Tree -> Tree) -> Model -> Result
doOneSelected f model =
    let
        path = model.selected |>
               Selection.first
    in liftMaybe (Silent "doOneSelected") path |>
    bind (\x -> doAt x f model)

doAt : Path -> (Tree -> Tree) -> Model -> Result
doAt path f model =
    let
        root = model.root
    in
        root |>
        Tree.get path |>
        liftMaybe (Silent "doAt 1") |>
        fmap f |>
        fmap (\x -> Tree.set path x root) |>
        flattenMaybe (Silent "doAt 2") |>
        fmap (refresh model)

{-| TODO
-}
clearSelection : Action
clearSelection m = succeed { m | selected = Selection.empty }

{-| TODO
-}
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

{-| TODO
-}
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
            otherwise -> fail (Silent "coIndex")

coIndex1: Path -> Model -> Result
coIndex1 = removeIndexAt

coIndex2 : Path -> Path -> Model -> Result
coIndex2 path1 path2 model =
    case Tree.root path1 == Tree.root path2 of
        False -> fail (Msg "Can't coindex nodes in two different roots")
        True ->
            let
                root = model.root
                tree1 = Tree.get path1 root
                tree2 = Tree.get path2 root
                index1 = tree1 ?> MT.datum ?> .index |> Maybe.Extra.join
                index2 = tree2 ?> MT.datum ?> .index |> Maybe.Extra.join
                ind = Tree.get (Tree.root path1) root ?>
                      Tree.highestIndex |>
                      Maybe.withDefault 0 |>
                      (+) 1
            in
                case (index1, index2) of
                    -- One of the nodes has an index, the other does not: set
                    -- the index of the unindexed node to match
                    (Nothing, Just x) -> setIndexAt path1 x.number model
                    (Just x, Nothing) -> setIndexAt path2 x.number model
                    -- Both of the nodes have an index: remove both indices
                    -- (TODO: toggle index types)
                    (Just x, Just y) ->
                        case (x.variety, y.variety) of
                            -- Normal coindexing -> gap
                            (Tree.Normal, Tree.Normal) ->
                                setIndexVarietyAt path2 Tree.Gap model
                            -- Gap -> backwards gap
                            (Tree.Normal, Tree.Gap) ->
                                setIndexVarietyAt path1 Tree.Gap model |>
                                bind (setIndexVarietyAt path2 Tree.Normal)
                            -- Backwards gap -> remove indexes
                            (Tree.Gap, Tree.Normal) ->
                                removeIndexAt path1 model |>
                                bind (removeIndexAt path2)
                            -- Something weird -> remove indexes (TODO: is
                            -- this right?)
                            otherwise -> removeIndexAt path1 model |>
                                         bind (removeIndexAt path2)
                    -- Neither node has an index -> coindex them
                    (Nothing, Nothing) -> setIndexAt path1 ind model |>
                                          bind (setIndexAt path2 ind)

setIndexAt: Path -> Int -> Action
setIndexAt path index =
    let
        f x = { x | index = Just { number = index, variety = Tree.Normal } }
    in
        doAt path (TX.updateDatum f)

setIndexVarietyAt : Path -> Tree.IndexVariety -> Action
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