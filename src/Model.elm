module Model exposing (Model
                      , withTrees
                      , doRoot
                      , maybeDoRoot
                      , root
                      , contextMenu
                      , selected
                      )

import Tree exposing (Tree, t)
import Selection
import ContextMenu
import Utils exposing (modifyMaybe)

import Monocle.Lens exposing (Lens, modify)

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

contextMenu : Lens Model ContextMenu.Model
contextMenu = Lens .contextMenu (\c m -> { m | contextMenu = c })

root : Lens Model Tree
root = Lens .root (\t m -> { m | root = t })

selected : Lens Model Selection.Selection
selected = Lens .selected (\s m -> { m | selected = s })

doRoot : (Tree -> Tree) -> Model -> Model
doRoot f =
    modify root f

maybeDoRoot : (Tree -> Maybe Tree) -> Model -> Model
maybeDoRoot f =
    modifyMaybe root f
