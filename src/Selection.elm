module Selection exposing (Selection(..) -- TODO: don't want to export contsructors
                          , first, second, one, two, empty,
                               -- doOne,
                          updateWith, get
                          , perform)

import Path exposing (Path)

-- import Utils exposing ((?>))

type Selection = None | One Path | Two Path Path

first : Selection -> Maybe Path
first s = case s of
              None -> Nothing
              One x -> Just x
              Two x _ -> Just x

second : Selection -> Maybe Path
second s = case s of
               None -> Nothing
               One _ -> Nothing
               Two _ x -> Just x

one : Path -> Selection
one = One

two : Path -> Path -> Selection
two = Two

empty : Selection
empty = None

updateWith : Path -> Selection -> Selection
updateWith p s = case s of
                     None -> One p
                     One f -> if p == f
                              then None
                              else Two f p
                     Two f s -> Two f p

-- TODO: not great, but needed for view functions
get : Selection -> List Path
get s = case s of
            None -> []
            One x -> [x]
            Two x y -> [x, y]

perform : Selection -> a -> (Path -> a) -> (Path -> Path -> a) -> a
perform sel none one two =
    case sel of
        None -> none
        One x -> one x
        Two x y -> two x y
