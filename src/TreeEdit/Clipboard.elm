module TreeEdit.Clipboard exposing (copy)

import Json.Decode as D
import Json.Encode as E

import RemoteData.Http as Http

import TreeEdit.Action exposing (Action)
import TreeEdit.Clipboard.Type exposing (..)
import TreeEdit.Model as Model
import TreeEdit.Msg exposing (Msg(Copy))
import TreeEdit.Result as R exposing (Result)
import TreeEdit.Tree as T
import TreeEdit.Tree.Encode exposing (encodeTree)
import TreeEdit.Selection as Selection

copy : Action
copy model =
    let
        selected = model |> .get Model.selected
        extract sel = T.get sel <| .get Model.root model
        tree = Selection.withOne selected extract Nothing
        treeVal = Maybe.map encodeTree tree
        requestData = Maybe.map (\x -> E.object [("tree", x)]) treeVal
        decoder = D.map3 Response (D.field "penn" D.string) (D.field "deep" D.string) (D.field "text" D.string)
        action = Maybe.map (Http.post "/as_text" Copy decoder) requestData
    in
        R.succeed model |> R.andDo (always (R.liftVal "copy" action))
