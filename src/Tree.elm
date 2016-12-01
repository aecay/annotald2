module Tree exposing (l, t, Tree, either, Index,
                          IndexVariety(..),
                          -- TODO: exporting all this internal stuff is not
                          -- the best...
                          TreeDatum, get, Path, set,
                          highestIndex
                     --, sameRoot
                     , root)

import List
import List.Extra exposing ((!!))
import Maybe

import Utils exposing ((?>?), (?>))

import MultiwayTree as T
import TreeExts as TX

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

type alias Path = List Int

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

either : (TreeDatum -> a) -> (TreeDatum -> a) -> Tree -> a
either nt t zipper =
    let
        d = T.datum zipper
        txt = d |> .text
    in
    case txt of
        Just _ -> t d
        Nothing -> nt d

get : Path -> Tree -> Maybe Tree
get path tree = case path of
                    [] -> Just tree
                    i :: is -> T.children tree |> flip (!!) i |> Maybe.andThen (get is)

set : Path -> Tree -> Tree -> Maybe Tree
set path newChild tree = case path of
                             [] -> Nothing
                             i :: [] -> TX.setChild i newChild tree
                             i :: is -> (T.children tree) !! i ?>?
                                        set is newChild ?>?
                                        \x -> TX.setChild i x tree

-- sameRoot : Path -> Path -> Bool
-- sameRoot a b = (root a) == (root b)

root : Path -> Path
root a = [Utils.fromJust (a !! 0)]

highestIndex : Tree -> Int
highestIndex t =
    let
        fold : TreeDatum -> Int -> Int
        fold d i = Maybe.withDefault 0 (d.index ?> .number) |> max i
    in
        T.foldl fold 0 t
