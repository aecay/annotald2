module TreeEdit.Msg exposing (Msg(..))

-- This is in a separate file not Update.elm so that we can import it other
-- places without cyclical dependencies

import Keyboard
import RemoteData exposing (WebData)

import TreeEdit.Config exposing (Config)
import TreeEdit.Path exposing (Path)
import TreeEdit.ContextMenuTypes as ContextMenuTypes
import TreeEdit.Model.Type as Model
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Metadata.Type as Metadata
import TreeEdit.View.LabelEdit.Type as Label

type Msg = ToggleSelect Path |
    KeyMsg Keyboard.KeyCode |
    RightClick Path ContextMenuTypes.Position |
    RightClickRoot |
    Context (ContextMenuTypes.Msg Model.Model) |
    LoadedData (WebData ((List Tree), Config)) |
    DoSave |
    LogMessage String |
    CancelContext |
    Metadata Metadata.Msg |
    Label Label.Msg |
    LabelKey Keyboard.KeyCode |
    Ignore
