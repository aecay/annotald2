module Model exposing (Model, withTrees, refresh)

import Tree exposing (Tree, t)
import Selection

type alias Model = { root: Tree
                   , selected: Selection.Selection
                   , lastMessage: String
                   }

withTrees : List Tree -> Model
withTrees trees = { root = t "WTF" trees
                  , selected = Selection.empty
                  , lastMessage = "Init"
                  }

refresh : Model -> Tree -> Model
refresh m t = { m | root = t }
