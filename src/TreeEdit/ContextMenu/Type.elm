module TreeEdit.ContextMenu.Type exposing (Model, Msg(..), Position)

import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Path exposing (Path)

type alias Position = { x: Int
                      , y: Int
                      }

type alias Model = { position : Position
                   , target : Path
                   }

type Msg =
    LeafBefore Path Tree |
    LeafAfter Path Tree |
    SetLabel Path String |
    ToggleExtension Path String |
    Ignore |
    Hide |
    Show Position Path
