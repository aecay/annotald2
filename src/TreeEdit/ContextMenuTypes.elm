module TreeEdit.ContextMenuTypes exposing (Model, Msg(..), Position, emptyModel)

import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Path exposing (Path)

type alias Position = { x: Int
                      , y: Int
                      }

-- TODO: should really just be one toplevel maybe value
type alias Model = { position : Position
                   , target : Maybe Path
                   }

emptyModel : Model
emptyModel = { position = { x = 0, y = 0 }, target = Nothing }

type Msg a =
    LeafBefore Path Tree |
    LeafAfter Path Tree |
    SetLabel Path String |
    ToggleExtension Path String |
    Ignore |
    Hide |
    Show Position Path
