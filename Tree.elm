module Tree exposing (l, t, Tree, TreeZipper, either, Index,
                          IndexVariety(..),
                          -- TODO: exporting all this internal stuff is not
                          -- the best...
                          TreeDatum)

import List

import MultiwayTree as T
import MultiwayTreeZipper as Z

-- Can partially unify these types by making a text: Maybe String key -> but
-- then the type system does not enforce the invariant that only children have
-- text (but this was already the case when we moved to using zippers) -> we
-- need our own zipper primitives for addChild etc. that enforce this
-- invariant
-- type alias Nonterminal_ = { label: String
--                           }


-- type alias Terminal_ = { text: String
--                        , label: String
--                        }

--  TODO: rather than manually maintaining, it would be better if this could
--  be autogenerated at compile time
-- type alias Common a = { a | label: String
--                       , selected: Bool
--                       }

type IndexVariety = Normal | Gap

type alias Index = { number: Int
                   , variety: IndexVariety
                   }

type alias TreeDatum = { text: Maybe String
                             -- TODO: we are relying on the code and not the
                             -- typechecker to maintain the invariant that
                             -- only terminal nodes have text
                       , label: String
                       , index: Maybe Index
                       }

type alias Tree = T.Tree TreeDatum

type alias TreeZipper = Z.Zipper TreeDatum

l : String -> String -> Tree
l label text = T.Tree { label = label
                      , text = Just text
                      , index = Nothing
                      }
               []

t : String -> List Tree -> Tree
t label children = T.Tree { label = label
                          , text = Nothing
                          , index = Nothing
                          }
                   children

mapChildren : (Tree -> a) -> Tree -> List a
mapChildren f t = List.map f (T.children t)

either : (TreeDatum -> a) -> (TreeDatum -> a) -> TreeZipper -> a
either nt t zipper =
    let
        d = Z.datum zipper
        txt = d |> .text
    in
    case txt of
        Just _ -> t d
        Nothing -> nt d

-- TODO: we have to manually write all these...is there a better way?  idea
-- which may not work: destructure both the type constructor and contents:
-- case d of (x y) -> x { y | y.selected = ... }
