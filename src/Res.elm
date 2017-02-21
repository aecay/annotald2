module Res exposing ( fail
                    , succeed
                    , failWarn
                    , andThen
                    , map
                    , map2
                    , Result
                    , withDefault
                    , lift
                    , liftWarn
                    , modify
                    , Failure (..)
                    )

-- A thin wrapper over the core Result type, specialized for string errors

import Result as R

import Monocle.Lens exposing (Lens)

import Maybe.Extra

-- This type represents failures during the execution of a user command.
-- The `Silent` failure type comes with a message, which is counterintuitive
-- but useful for debugging.

type Failure = Fail String | Warn String

-- This type represents the result of applying a user command.

-- The command can succeed with a new model (which replaces the old one), or
-- fail with a `Failure`.

type alias Result a = R.Result Failure a

fail : String -> Result a
fail s = R.Err (Fail s)

failWarn : String -> Result a
failWarn s = R.Err (Warn s)

succeed : a -> Result a
succeed = R.Ok

andThen : (a -> Result b) -> Result a -> Result b
andThen = R.andThen

map : (a -> b) -> Result a -> Result b
map = R.map

map2 : (a -> b -> c) -> Result a -> Result b -> Result c
map2 = R.map2

withDefault : a -> Result a -> a
withDefault default result =
    case result of
        R.Ok x -> x
        R.Err _ -> default

lift : String -> Maybe a -> Result a
lift err val = Maybe.Extra.unwrap (fail err) succeed val

liftWarn : String -> Maybe a -> Result a
liftWarn err val = Maybe.Extra.unwrap (failWarn err) succeed val

modify : Lens a b -> (b -> Result b) -> a -> a
modify lens f init =
    let
        init1 = lens.get init
    in
        init1 |> f |> map (flip lens.set init) |> withDefault init
