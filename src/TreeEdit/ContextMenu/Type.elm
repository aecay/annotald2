module TreeEdit.ContextMenu.Type exposing (Model, Msg(..), Position)

import TreeEdit.Path exposing (Path)
import TreeEdit.Tree.Type exposing (Tree)


type alias Position =
    { x : Int
    , y : Int
    }


type alias Model =
    { position : Position
    , target : Path
    }


type Msg
    = LeafBefore Path Tree
    | LeafAfter Path Tree
    | SetLabel Path String
    | ToggleExtension Path String
    | Ignore
    | Hide
    | Show Position Path
