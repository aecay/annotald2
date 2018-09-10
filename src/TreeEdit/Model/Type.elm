module TreeEdit.Model.Type exposing (Model, ForestModel)

import Html exposing (Html)
import Random exposing (Seed)
import RemoteData exposing (WebData)
import TreeEdit.Config exposing (Config)
import TreeEdit.ContextMenu.Type as ContextMenuType
import TreeEdit.Dialog.Type exposing (Dialog)
import TreeEdit.Metadata.Type as Metadata
import TreeEdit.Msg as Msg
import TreeEdit.Path as Path
import TreeEdit.Selection as Selection
import TreeEdit.Tree.Type exposing (Forest, Tree)
import TreeEdit.View.LabelEdit.Type exposing (LabelForm)

type alias ForestModel = { root : Forest
                         , config : Config
                         , lemmata : List Metadata.Lemma
                         , selected : Selection.Selection
                         , contextMenu : Maybe ContextMenuType.Model
                         , metadataForm : Maybe Metadata.Model
                         , labelForm : Maybe LabelForm
                         , undo : List Forest
                         , redo : List Forest
                         , seed : Seed
                         }


type alias Model =
    { lastMessage : String
    , fileName : String
    , webdata : WebData ForestModel
    , dialog : Maybe Dialog
    , dirty : Bool
    , seed : Seed
    }
