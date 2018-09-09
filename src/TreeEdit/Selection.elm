module TreeEdit.Selection exposing
    ( Selection
    , empty
    , first
    , get
    , getOne
    , one
    , perform
    , second
    , two
    , updateWith
    , withOne
    , withTwo
    )

import TreeEdit.Path exposing (Path)


type Selection
    = None
    | One Path
    | Two Path Path


first : Selection -> Maybe Path
first s =
    case s of
        None ->
            Nothing

        One x ->
            Just x

        Two x _ ->
            Just x


second : Selection -> Maybe Path
second s =
    case s of
        None ->
            Nothing

        One _ ->
            Nothing

        Two _ x ->
            Just x


one : Path -> Selection
one =
    One


two : Path -> Path -> Selection
two =
    Two


empty : Selection
empty =
    None


updateWith : Path -> Selection -> Selection
updateWith p s =
    case s of
        None ->
            One p

        One f ->
            if p == f then
                None

            else
                Two f p

        Two f _ ->
            Two f p



-- TODO: not great, but needed for view functions


get : Selection -> List Path
get s =
    case s of
        None ->
            []

        One x ->
            [ x ]

        Two x y ->
            [ x, y ]


getOne : Selection -> Maybe Path
getOne s =
    case s of
        None ->
            Nothing

        One x ->
            Just x

        Two _ _ ->
            Nothing


perform : Selection -> a -> (Path -> a) -> (Path -> Path -> a) -> a
perform sel none one_ two_ =
    case sel of
        None ->
            none

        One x ->
            one_ x

        Two x y ->
            two_ x y



-- TODO: does it make sense to pass default as an additional argument to fn?
-- i.e. type singature becomes Selection -> (Path -> a -> a) -> a -> a, and
-- the crucial bit is fn x default


withOne : Selection -> (Path -> a) -> a -> a
withOne sel fn default =
    case sel of
        One x ->
            fn x

        _ ->
            default


withTwo : Selection -> (Path -> Path -> a) -> a -> a
withTwo sel fn default =
    case sel of
        Two x y ->
            fn x y

        _ ->
            default
