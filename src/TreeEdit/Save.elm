module TreeEdit.Save exposing (failure, perform, success)

import Json.Decode as D
import Json.Encode as E
import Http

import Cmd.Extra
import RemoteData exposing (RemoteData(..))
import Return exposing (Return, command, singleton)
import Url.Builder exposing (absolute)

import TreeEdit.Dialog.Type exposing (Dialog(..))
import TreeEdit.Model.Type exposing (Model, ForestModel)
import TreeEdit.Msg exposing (LoadedMsg(..), Msg(..))
import TreeEdit.Tree.Encode exposing (encodeForest)
import TreeEdit.Utils as Utils exposing (message)

perform : String -> ForestModel -> Return Msg ForestModel
perform filename forestModel =
    let
        root = forestModel.root
        handle : Result Http.Error () -> Msg
        handle d =
            case d of
                Ok _ -> SaveSuccess
                Err e -> SaveFailure <| RemoteData.httpErrorToString e
    in
        Return.return forestModel <|
            Cmd.batch [ Cmd.Extra.perform <| SetDialog <| Just <| Processing "saving"
                      , Http.post { url = absolute ["save"] []
                                  , body = Http.jsonBody (E.object
                                                              [ ( "filename", E.string filename )
                                                              , ( "trees", encodeForest root )
                                                              ]
                                                         )
                                  , expect = Http.expectWhatever handle
                                  }
                      ]


success : Model -> Return Msg Model
success model =
    singleton model
      |> message (LogMessage "Save success")
      |> message (Dirty False)
      |> message (SetDialog Nothing)


failure : Model -> String -> Return Msg Model
failure model reason =
    singleton model
      |> message (LogMessage <| "Save failure: " ++ reason)
      |> message (SetDialog Nothing)
