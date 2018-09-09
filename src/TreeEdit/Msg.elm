module TreeEdit.Msg exposing (Msg(..))

import Array exposing (Array)
import RemoteData exposing (WebData)
import ThirdParty.KeyboardEvent exposing (KeyboardEvent)
import TreeEdit.Clipboard.Type as Clipboard
import TreeEdit.Config exposing (Config)
import TreeEdit.ContextMenu.Type as ContextMenuType
import TreeEdit.Metadata.Type as Metadata
import TreeEdit.Path exposing (Path)
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.View.LabelEdit.Type as Label


type Msg
    = ToggleSelect Path
    | KeyMsg KeyboardEvent
    | RightClick Path ContextMenuType.Position
    | RightClickRoot
    | Context ContextMenuType.Msg
    | LoadedData (WebData ( Array Tree, Config, List Metadata.Lemma ))
    | Save
    | SaveSuccess
    | SaveFailure String
    | LogMessage String
    | CancelContext
    | Metadata Metadata.Msg
    | Label Label.Msg
    | LabelKey KeyboardEvent
    | Copy (WebData Clipboard.Response)
    | DismissDialog
    | Validate
    | ValidateDone (Maybe Path) (WebData (Array Tree))
    | -- FixValidator |
      -- FixValidatorDone (WebData ()) |
      Ignore
    | Undo
    | Redo
    | Dirty Bool
    | Blur String
    | Exit
