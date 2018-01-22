module TreeEdit.Save exposing (perform, success, failure)

import Json.Decode as D
import Json.Encode as E

import Cmd.Extra as CX
import RemoteData exposing (RemoteData(Success))
import RemoteData.Http
import Return exposing (Return)

import TreeEdit.Dialog exposing (Dialog(Processing))
import TreeEdit.Model as Model exposing (Model)
import TreeEdit.Msg exposing (Msg(SaveFailure, SaveSuccess, LogMessage))
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Encode exposing (encodeTrees)
import TreeEdit.Utils as Utils

perform : Model -> Return Msg Model
perform model =
    let
        root = .get Model.root model
        handle : (RemoteData.WebData () -> Msg)
        handle d = case d of
                       Success _ -> SaveSuccess
                       f -> SaveFailure <| toString f
    in
        Return.return {model | dialog = Just <| Processing "saving" } <|
            RemoteData.Http.post
                "/save"
                handle
                (D.succeed ())
                (E.object [ ("filename", E.string model.fileName)
                          , ("trees", encodeTrees <| Utils.fromJust <| .getOption Tree.children root)
                          ])

success : Model -> Return Msg Model
success model =
    Return.return { model | dialog = Nothing } (CX.perform <| LogMessage "Save success")

failure : Model -> String -> Return Msg Model
failure model reason =
    Return.return { model | dialog = Nothing } (CX.perform <| LogMessage <| "Save failure: " ++ reason)
