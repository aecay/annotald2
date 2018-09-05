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
        decoder = D.map3 Response (D.field "penn" D.string) (D.field "deep" D.string) (D.field "text" D.string)
        extract sel = .get Model.root model |>
                      T.get sel |>
                      encodeTree |>
                      (\x -> E.object [("tree", x)]) |>
                      Http.post "/as_text" Copy decoder
        cmd = Selection.perform selected
              Cmd.none
              extract
              (\_ _ -> Cmd.none)
    in
        R.succeed model |> R.do cmd
