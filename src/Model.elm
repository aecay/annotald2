module Model exposing (Model, withTrees, refresh)

import Tree exposing (Tree, t)
import Selection

type alias Model = { root: Tree
                   , selected: Selection.Selection
                   }

withTrees : List Tree -> Model
withTrees trees = { root = t "WTF" trees
                  , selected = Selection.empty
                  }

refresh : Model -> Tree -> Model
refresh m t = { m | root = t }
