module Actions exposing ( clearSelection
                        , changeLabel
                        , Action
                        )

import Maybe exposing (withDefault)
--import List.Extra exposing (singleton)
--import List exposing (head, tail)

import Tree exposing (Path, Tree)

import Utils exposing ((?>), (?>?), zip)

import Dict exposing (Dict)

import TreeExts as TX

import Model exposing (Model, refresh)
import Selection


type alias Action = Model -> Maybe Model

doOneSelected : (Tree -> Tree) -> Model -> Maybe Model
doOneSelected f model =
    let
        path = model.selected |>
               Selection.first
    in path ?>?
    \x -> doAt x f model

doAt : Path -> (Tree -> Tree) -> Model -> Maybe Model
doAt path f model =
    let
        root = model.root
    in
        root |>
        Tree.get path ?>
        f ?>?
        \x -> Tree.set path x root ?>?
        refresh model >> Just

clearSelection : Action
clearSelection m = Just { m | selected = Selection.empty }

changeLabel : List String -> Action
changeLabel labels =
    case labels of
        [] -> Just
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

-- coIndex : Action
-- coIndex m =
--     m.selected

-- TODO: problem...we want to update both zippers, but only one of them will
-- "win" when we push it back to the model

-- Solution: we'll have to go back to a path-like solution
