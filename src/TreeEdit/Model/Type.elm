module TreeEdit.Model.Type exposing (Model)

import RemoteData exposing (WebData)

import TreeEdit.Config exposing (Config)
import TreeEdit.ContextMenuTypes as ContextMenuTypes
import TreeEdit.Metadata.Type as Metadata
import TreeEdit.Selection as Selection
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.View.LabelEdit.Type exposing (LabelForm)

type alias Model = { selected: Selection.Selection
                   , lastMessage: String
                   , contextMenu: ContextMenuTypes.Model
                   , fileName : String
                   , metadataForm : Maybe (Metadata.MetadataForm, Metadata.FieldState)
                   , webdata : WebData (Tree, Config)
                   , labelForm: Maybe LabelForm
                   }
