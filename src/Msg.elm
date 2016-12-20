module Msg exposing (Msg(..))

-- This is in a separate file not Update.elm so that we can import it other
-- places without cyclical dependencies

import Path exposing (Path)
import Keyboard
import ContextMenu
import Model

type Msg = ToggleSelect Path |
    KeyMsg Keyboard.KeyCode |
    RightClick Path ContextMenu.Position |
    Context (ContextMenu.Msg Model.Model)
