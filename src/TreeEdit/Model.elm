module TreeEdit.Model exposing ( Model
                               , withTrees
                               , doRoot
                               , root
                               , contextMenu
                               , selected
                      )

import Monocle.Lens exposing (Lens, modify)
import RemoteData exposing (WebData, RemoteData(..))

import TreeEdit.Tree exposing (Tree, t)
import TreeEdit.Selection as Selection
import TreeEdit.ContextMenuTypes as ContextMenuTypes
import TreeEdit.Res as R

type alias Model = { root: WebData Tree
                   , selected: Selection.Selection
                   , lastMessage: String
                   , contextMenu: ContextMenuTypes.Model
                   , fileName : String
                   , message : String
                   }

withTrees : List Tree -> String -> Model
withTrees trees s = { root = Success <| t "WTF" trees
                    , selected = Selection.empty
                    , lastMessage = "Init"
                    , contextMenu = ContextMenuTypes.emptyModel
                    , fileName = s
                    , message = ""
                    }

contextMenu : Lens Model ContextMenuTypes.Model
contextMenu = Lens .contextMenu (\c m -> { m | contextMenu = c })

root : Lens Model Tree
root =
    let
        get m = case m.root of
                    Success tree -> tree
                    _ -> Debug.crash "Tried to get the root when no data was loaded"
        set tree m = case m.root of
                         Success _ -> { m | root = Success tree }
                         _ -> Debug.crash "Tried to set the root when no data was loaded"
    in
        Lens get set

selected : Lens Model Selection.Selection
selected = Lens .selected (\s m -> { m | selected = s })

doRoot : (Tree -> R.Result Tree) -> Model -> Model
doRoot f =
    R.modify root f
