module TreeEdit.Msg exposing (Msg(..))

import Keyboard.Event exposing (KeyboardEvent)
import RemoteData exposing (WebData)

import TreeEdit.Config exposing (Config)
import TreeEdit.Path exposing (Path)
import TreeEdit.ContextMenuTypes as ContextMenuTypes
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Metadata.Type as Metadata
import TreeEdit.View.LabelEdit.Type as Label

type Msg = ToggleSelect Path |
    KeyMsg KeyboardEvent |
    RightClick Path ContextMenuTypes.Position |
    RightClickRoot |
    Context ContextMenuTypes.Msg |
    LoadedData (WebData (List Tree, Config, List Metadata.Lemma)) |
    Save |
    SaveSuccess |
    SaveFailure String |
    LogMessage String |
    CancelContext |
    Metadata Metadata.Msg |
    Label Label.Msg |
    LabelKey KeyboardEvent |
    Copy (WebData String) |
    DismissDialog |
    Validate |
    ValidateDone (WebData (List Tree)) |
    -- FixValidator |
    -- FixValidatorDone (WebData ()) |
    Ignore |
    Undo |
    Redo
