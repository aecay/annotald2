module TreeEdit.Save exposing (perform, success, failure)

import Json.Decode as D
import Json.Encode as E

import RemoteData exposing (RemoteData(Success))
import RemoteData.Http
import Return exposing (Return, singleton, command)

import TreeEdit.Dialog exposing (Dialog(Processing))
import TreeEdit.Model as Model exposing (Model)
import TreeEdit.Msg exposing (Msg(SaveFailure, SaveSuccess, LogMessage, Dirty))
import TreeEdit.Tree.Encode exposing (encodeForest)
import TreeEdit.Utils as Utils exposing (message, fromJust)

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
                          , ("trees", encodeForest root)
                          ])

success : Model -> Return Msg Model
success model =
    singleton { model | dialog = Nothing } |>
    message (LogMessage "Save success") |>
    message (Dirty False)

failure : Model -> String -> Return Msg Model
failure model reason =
    Return.singleton { model | dialog = Nothing } |>
    message (LogMessage <| "Save failure: " ++ reason)
