module TreeEdit.Msg exposing (Msg(..))

import Array.Hamt as Array exposing (Array)
import Keyboard.Event exposing (KeyboardEvent)
import RemoteData exposing (WebData)

import TreeEdit.Config exposing (Config)
import TreeEdit.Clipboard.Type as Clipboard
import TreeEdit.Path exposing (Path)
import TreeEdit.ContextMenu.Type as ContextMenuType
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Metadata.Type as Metadata
import TreeEdit.View.LabelEdit.Type as Label

type Msg = ToggleSelect Path |
    KeyMsg KeyboardEvent |
    RightClick Path ContextMenuType.Position |
    RightClickRoot |
    Context ContextMenuType.Msg |
    LoadedData (WebData (Array Tree, Config, List Metadata.Lemma)) |
    Save |
    SaveSuccess |
    SaveFailure String |
    LogMessage String |
    CancelContext |
    Metadata Metadata.Msg |
    Label Label.Msg |
    LabelKey KeyboardEvent |
    Copy (WebData Clipboard.Response) |
    DismissDialog |
    Validate |
    ValidateDone (WebData (Array Tree)) |
    -- FixValidator |
    -- FixValidatorDone (WebData ()) |
    Ignore |
    Undo |
    Redo |
    Dirty Bool |
    Exit
