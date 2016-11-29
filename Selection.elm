module Selection exposing (Selection, first, second, one, two, empty, doOne,
                          updateWith, get)

import Tree exposing (TreeZipper)

import Utils exposing ((?>))

type Selection = None | One TreeZipper | Two TreeZipper TreeZipper

first : Selection -> Maybe TreeZipper
first s = case s of
              None -> Nothing
              One x -> Just x
              Two x _ -> Just x

second : Selection -> Maybe TreeZipper
second s = case s of
               None -> Nothing
               One _ -> Nothing
               Two _ x -> Just x

one : TreeZipper -> Selection
one = One

two : TreeZipper -> TreeZipper -> Selection
two = Two

empty : Selection
empty = None

doOne : (TreeZipper -> TreeZipper) -> Selection -> Maybe TreeZipper
doOne f s = first s ?> f

updateWith : TreeZipper -> Selection -> Selection
updateWith z s = case s of
                     None -> One z
                     One f -> if z == f
                              then None
                              else Two f z
                     Two f s -> Two f z

-- TODO: not great, but needed for view functions
get : Selection -> List TreeZipper
get s = case s of
            None -> []
            One x -> [x]
            Two x y -> [x, y]
