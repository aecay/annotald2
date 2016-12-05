module Model exposing (Model
                      , withTrees
                      , refresh
                      , root
                      , contextMenu
                      , selected
                      )

import Tree exposing (Tree, t)
import Selection
import ContextMenu

import Monocle.Lens exposing (Lens)

type alias Model = { root: Tree
                   , selected: Selection.Selection
                   , lastMessage: String
                   , contextMenu: ContextMenu.Model
                   }

withTrees : List Tree -> Model
withTrees trees = { root = t "WTF" trees
                  , selected = Selection.empty
                  , lastMessage = "Init"
                  , contextMenu = ContextMenu.emptyModel
                  }

refresh : Model -> Tree -> Model
refresh m t = { m | root = t }

contextMenu : Lens Model ContextMenu.Model
contextMenu = Lens .contextMenu (\c m -> { m | contextMenu = c })

root : Lens Model Tree
root = Lens .root (\t m -> { m | root = t })

selected : Lens Model Selection.Selection
selected = Lens .selected (\s m -> { m | selected = s })
