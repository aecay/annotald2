module TreeEdit.Model.Type exposing (Model)

import Html exposing (Html)
import RemoteData exposing (WebData)

import TreeEdit.Config exposing (Config)
import TreeEdit.ContextMenuTypes as ContextMenuTypes
import TreeEdit.Dialog exposing (Dialog)
import TreeEdit.Metadata.Type as Metadata
import TreeEdit.Selection as Selection
import TreeEdit.Msg as Msg
import TreeEdit.Path as Path
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.View.LabelEdit.Type exposing (LabelForm)

type alias Model = { selected: Selection.Selection
                   , lastMessage: String
                   , contextMenu: ContextMenuTypes.Model
                   , fileName : String
                   , metadataForm : Maybe Metadata.Model
                   , webdata : WebData { root : Tree
                                       , config : Config
                                       , viewFn : (Maybe (List Path.Path, Maybe LabelForm) ->
                                                       Int -> Tree -> Html Msg.Msg)
                                       , lemmata : List Metadata.Lemma
                                       }
                   , labelForm: Maybe LabelForm
                   , dialog : Maybe Dialog
                   , undo : List Tree
                   , redo: List Tree
                   }
