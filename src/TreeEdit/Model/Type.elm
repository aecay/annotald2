module TreeEdit.Model.Type exposing (Model)

import Html exposing (Html)
import Random exposing (Seed)
import RemoteData exposing (WebData)
import TreeEdit.Config exposing (Config)
import TreeEdit.ContextMenu.Type as ContextMenuType
import TreeEdit.Dialog exposing (Dialog)
import TreeEdit.Metadata.Type as Metadata
import TreeEdit.Msg as Msg
import TreeEdit.Path as Path
import TreeEdit.Selection as Selection
import TreeEdit.Tree.Type exposing (Forest, Tree)
import TreeEdit.View.LabelEdit.Type exposing (LabelForm)


type alias Model =
    { selected : Selection.Selection
    , lastMessage : String
    , contextMenu : Maybe ContextMenuType.Model
    , fileName : String
    , metadataForm : Maybe Metadata.Model
    , webdata :
        WebData
            { root : Forest
            , config : Config
            , viewFn :
                Maybe ( List Path.Path, Maybe LabelForm )
                ->
                    -- TODO: use proper Id
                    -- type instead of String
                    String
                -> Tree
                -> Html Msg.Msg
            , lemmata : List Metadata.Lemma
            }
    , labelForm : Maybe LabelForm
    , dialog : Maybe Dialog
    , undo : List Forest
    , redo : List Forest
    , dirty : Bool
    , seed : Seed
    }
