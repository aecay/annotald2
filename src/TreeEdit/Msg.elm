module TreeEdit.Msg exposing (Msg(..))

-- This is in a separate file not Update.elm so that we can import it other
-- places without cyclical dependencies

import Keyboard
import RemoteData exposing (WebData)

import TreeEdit.Path exposing (Path)
import TreeEdit.ContextMenuTypes as ContextMenuTypes
import TreeEdit.Model as Model
import TreeEdit.Tree exposing (Tree)
import TreeEdit.Metadata.Type as Metadata

type Msg = ToggleSelect Path |
    KeyMsg Keyboard.KeyCode |
    RightClick Path ContextMenuTypes.Position |
    RightClickRoot |
    Context (ContextMenuTypes.Msg Model.Model) |
    GotTrees (WebData (List Tree)) |
    DoSave |
    LogMessage String |
    CancelContext |
    Metadata Metadata.Msg
