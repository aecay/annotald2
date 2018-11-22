module TreeEdit.Save exposing (failure, perform, success)

import Json.Decode as D
import Json.Encode as E

import Cmd.Extra
import RemoteData exposing (RemoteData(..))
import RemoteData.Http
import Return exposing (Return, command, singleton)

import TreeEdit.Dialog.Type exposing (Dialog(..))
import TreeEdit.Model.Type exposing (Model, ForestModel)
import TreeEdit.Msg exposing (LoadedMsg(..), Msg(..))
import TreeEdit.Tree.Encode exposing (encodeForest)
import TreeEdit.Utils as Utils exposing (message)

import Util exposing (webDataToString)

perform : String -> ForestModel -> Return Msg ForestModel
perform filename forestModel =
    let
        root = forestModel.root
        handle : RemoteData.WebData () -> Msg
        handle d =
            case d of
                Success _ -> SaveSuccess
                _ -> SaveFailure <| webDataToString d
    in
        Return.return forestModel <|
            Cmd.batch [ Cmd.Extra.perform <| SetDialog <| Just <| Processing "saving"
                      , RemoteData.Http.post
                          "/save"
                              handle
                              (D.succeed ())
                              (E.object
                                   [ ( "filename", E.string filename )
                                   , ( "trees", encodeForest root )
                                   ]
                              )
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
