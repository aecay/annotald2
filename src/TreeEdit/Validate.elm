module TreeEdit.Validate exposing (done, perform)

import Array exposing (Array)
import Cmd.Extra
import Http
import Json.Encode as E
import RemoteData exposing (RemoteData(..), WebData)
import Return exposing (Return)
import Url.Builder exposing (absolute)
import TreeEdit.Dialog.Type exposing (Dialog(..))
import TreeEdit.Metadata.Type as MetadataType
import TreeEdit.Model.Type exposing (Model, ForestModel)
import TreeEdit.Msg as Msg exposing (Msg)
import TreeEdit.OrderedDict as OD
import TreeEdit.Path as Path exposing (Path)
import TreeEdit.Selection as Selection
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Decode exposing (decodeTrees)
import TreeEdit.Tree.Encode exposing (encodeTrees)
import TreeEdit.Tree.Type as TreeType exposing (Tree)
import TreeEdit.Utils as Utils

perform : ForestModel -> Return Msg ForestModel
perform forestModel =
    let
        root = forestModel.root
        sel = forestModel.selected |> Selection.getOne |> Maybe.map Path.root
        allTrees : Array Tree
        allTrees = OD.values root
        trees = sel
              |> Maybe.map (\a -> Tree.get a root)
              |> Maybe.map (Array.repeat 1)
              |> Maybe.withDefault allTrees
    in
        Return.return forestModel <|
            Cmd.batch [ Cmd.Extra.perform <| Msg.SetDialog <| Just <| Processing "Validating"
                      , Http.post { url = absolute ["validate"] []
                                  , body = Http.jsonBody <| E.object [ ( "trees", encodeTrees trees ) ]
                                  , expect = Http.expectJson
                                             (Msg.Loaded << (Msg.ValidateDone sel) << RemoteData.fromResult)
                                             decodeTrees
                                  }
                      ]


done : ForestModel -> Maybe Path -> WebData (Array Tree) -> Return Msg ForestModel
done forestModel path webdata =
    case webdata of
        Success trees ->
            let
                newRoot =
                    case path of
                        Just p ->
                            let
                                root = forestModel.root
                                oldTree = Tree.get p root
                                newTree = Array.get 0 trees |> Maybe.withDefault oldTree
                            in
                                Tree.set p newTree root
                        Nothing ->
                            -- TODO: toList is bogus here, it's a
                            -- needless roundtrip through the List type
                            Tree.forestFromList <| Array.toList trees
            in
                Return.return { forestModel | root = newRoot } <|
                    Utils.cmds
                    [ Msg.Loaded <| Msg.Metadata MetadataType.NewSelection
                    , Msg.SetDialog Nothing
                    , Msg.LogMessage "Validate success"
                    ]
        f ->
            Return.return forestModel <|
                Utils.cmds [ Msg.LogMessage <| "Validation failure: " ++ RemoteData.toString f
                           , Msg.SetDialog Nothing
                           ]
