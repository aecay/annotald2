module TreeEdit.Model exposing ( Model
                               , init
                               , doRoot
                               , root
                               , contextMenu
                               , selected
                      )

import Monocle.Lens exposing (Lens, modify)
import RemoteData exposing (WebData, RemoteData(..))

import TreeEdit.Config exposing (Config)
import TreeEdit.Tree exposing (Tree)
import TreeEdit.Selection as Selection
import TreeEdit.ContextMenuTypes as ContextMenuTypes
import TreeEdit.Result as R
import TreeEdit.Metadata.Type as Metadata

type alias Model = { selected: Selection.Selection
                   , lastMessage: String
                   , contextMenu: ContextMenuTypes.Model
                   , fileName : String
                   , metadataForm : Maybe (Metadata.MetadataForm, Metadata.FieldState)
                   , webdata : WebData (Tree, Config)
                   , editingLemma : Bool
                   }

init : String -> Model
init filename = { webdata = NotAsked
                , selected = Selection.empty
                , lastMessage = "Init"
                , contextMenu = ContextMenuTypes.emptyModel
                , fileName = filename
                , metadataForm = Nothing
                , editingLemma = False
                }

contextMenu : Lens Model ContextMenuTypes.Model
contextMenu = Lens .contextMenu (\c m -> { m | contextMenu = c })

root : Lens Model Tree
root =
    let
        get m = case m.webdata of
                    Success (root, _) -> root
                    _ -> Debug.crash "Tried to get the root when no data was loaded"
        set tree m = case m.webdata of
                         Success (_, config) -> { m | webdata = Success (tree, config) }
                         _ -> Debug.crash "Tried to set the root when no data was loaded"
    in
        Lens get set

selected : Lens Model Selection.Selection
selected = Lens .selected (\s m -> { m | selected = s })

doRoot : (Tree -> R.Result Tree) -> Model -> Model
doRoot = R.modify root
