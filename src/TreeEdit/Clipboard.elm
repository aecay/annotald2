module TreeEdit.Clipboard exposing (copy)

import Json.Decode as D
import Json.Encode as E

import RemoteData.Http as Http

import TreeEdit.Actions exposing (Action)
import TreeEdit.Model as Model
import TreeEdit.Msg exposing (Msg(Copy))
import TreeEdit.Result as R exposing (Result)
import TreeEdit.Tree as T
import TreeEdit.Tree.Json as TJ
import TreeEdit.Selection as Selection

copy : Action
copy model =
    let
        selected = model |> .get Model.selected
        extract sel = T.get sel <| .get Model.root model
        tree = Selection.withOne selected extract <| R.fail "foo"
        treeVal = R.map ((\x -> [x]) >> TJ.toJson) tree
        requestData = R.map (\x -> E.object [("tree", x)]) treeVal
        action = R.map (Http.post "/as_text" Copy D.string) requestData
    in
        R.succeed model |> R.andDo (always action)
