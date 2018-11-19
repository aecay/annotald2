module TreeEdit.Msg exposing (Msg(..), LoadedMsg(..))

import Array exposing (Array)
import RemoteData exposing (WebData)
import ThirdParty.KeyboardEvent exposing (KeyboardEvent)
import TreeEdit.Clipboard.Type as Clipboard
import TreeEdit.Config exposing (Config)
import TreeEdit.ContextMenu.Type as ContextMenuType
import TreeEdit.Dialog.Type exposing (Dialog)
import TreeEdit.Metadata.Type as Metadata
import TreeEdit.Path exposing (Path)
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.View.LabelEdit.Type as Label

type LoadedMsg =
    ToggleSelect Path
    | KeyMsg KeyboardEvent
    | RightClick Path ContextMenuType.Position
    | RightClickRoot
    | Context ContextMenuType.Msg
    | CancelContext
    | Metadata Metadata.Msg
    | Label Label.Msg
    | LabelKey KeyboardEvent
    | Copy (WebData Clipboard.Response)
    | Validate
    | ValidateDone (Maybe Path) (WebData (Array Tree))
    | Undo
    | Redo


type Msg
    = LoadedData (WebData ( Array Tree, Config, List String ))
    | LogMessage String
    | Ignore
    | Blur String
    | Exit
    | Loaded LoadedMsg
    | SetDialog (Maybe Dialog)
    | Save
    | SaveSuccess
    | SaveFailure String
    | Dirty Bool
