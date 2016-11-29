module Actions exposing (clearSelection, changeLabel, Action)

import Maybe exposing (withDefault)
--import List.Extra exposing (singleton)
--import List exposing (head, tail)

import Dict exposing (Dict)

import Utils exposing ((?>), zip)

import Model exposing (Model, refresh)
import Selection

import ZipperExts as ZX
import Tree exposing (TreeZipper)

type alias Action = Model -> Maybe Model

doSelected : (TreeZipper -> TreeZipper) -> Model -> Maybe Model
doSelected f m =
    m.selected |>
    Selection.doOne f ?>
    refresh m

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
                update : TreeZipper -> TreeZipper
                update z = ZX.updateDatum (\d -> { d | label = change d.label }) z
            in doSelected update

-- coIndex : Action
-- coIndex m =
--     m.selected

-- TODO: problem...we want to update both zippers, but only one of them will
-- "win" when we push it back to the model

-- Solution: we'll have to go back to a path-like solution
