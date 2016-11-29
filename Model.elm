module Model exposing (Model, withTrees, refresh)

import Tree exposing (Tree, t, TreeZipper)
import Selection

import ZipperExts exposing (unzip, goToRoot_)

type alias Model = { root: Tree
                   , selected: Selection.Selection
                   }

type alias Selection = (Maybe TreeZipper, Maybe TreeZipper)

withTrees : List Tree -> Model
withTrees trees = { root = t "WTF" trees
                  , selected = Selection.empty
                  }

-- refreshTrees : Model -> TreeZipper -> Model
-- refreshTrees m z = { m | root = z |> goToRoot_ |> unzip }

refresh : Model -> TreeZipper -> Model
refresh m z = { m | root = z |> goToRoot_ |> unzip
              , selected = Selection.one z
              }

-- TODO: after refreshTrees (the old version above), the selection zipper
-- becomes stale (since it wraps the whole, now changed, tree) Option 1: make
-- refreshTrees set the selection as well.  Option 2: turn selection into a
-- path (list of Ints indexing which children to go to) rather than a zipper.
-- Disadvantage of option 1 is that the selection becomes the only way to
-- change the tree (but we can provide a setTrees method that clears the
-- selection).  Disadvantage of option 2 is that changes to the tree might
-- invalidate the selection (if we insert a sibling before it, e.g.)

-- THe old annotald approach was to store selected as a property on the node,
-- and then to search through the tree every time we needed it.  This was fast
-- because we used DOM structures to store the node.  But we don't want to
-- proceed in this way any more.  (Crazy idea: write a wrapper that uses a
-- (uninserted in the document) DOM tree to hold our data and expose it in a
-- sane way)
