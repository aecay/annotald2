module TreeEdit.Clipboard exposing (copy)

import Http
import Json.Decode as D
import Json.Encode as E
import Url.Builder exposing (absolute)

import RemoteData
import TreeEdit.Action exposing (Action)
import TreeEdit.Clipboard.Type exposing (..)
import TreeEdit.Model as Model
import TreeEdit.Msg exposing (LoadedMsg(..), Msg(..))
import TreeEdit.Result as R exposing (Result)
import TreeEdit.Selection as Selection
import TreeEdit.Tree as T
import TreeEdit.Tree.Encode exposing (encodeTree)


copy : Action
copy model =
    let
        selected = model.selected
        decoder = D.map3 Response
                  (D.field "penn" D.string)
                  (D.field "deep" D.string)
                  (D.field "text" D.string)
        url = absolute ["as_text"] []
        extract sel =
            model.root
                |> T.get sel
                |> encodeTree
                |> (\x -> E.object [ ( "tree", x ) ])
                |> (\x -> Http.post { url = url
                                    , body = Http.jsonBody x
                                    , expect = Http.expectJson (Loaded << Copy << RemoteData.fromResult) decoder
                                    })

        cmd =
            Selection.perform selected
                Cmd.none
                extract
                (\_ _ -> Cmd.none)
    in
        R.succeed model |> R.do cmd
