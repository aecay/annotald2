module TreeEdit.Result exposing ( fail
                                , succeed
                                , failWarn
                                , andThen
                                , map
                                , map2
                                , Result(..)
                                , withDefault
                                , lift
                                , modify
                                , guard
                                -- , Failure(..)
                                , foldr
                                , andMap
                                -- , map3
                                , andThen3
                             )

import Maybe.Extra
import Monocle.Lens exposing (Lens)

type alias Message = String

type Result a = Result (List Message) (Maybe a)

fail : String -> Result a
fail s = Result [s] Nothing

guard : Result Bool -> String -> Result a -> Result a
guard flag failMsg res =
    case flag of
        Result msgs (Just True) -> res
        Result msgs _ -> Result (msgs ++ [failMsg]) Nothing

-- TODO: remove
failWarn : String -> Result a
failWarn = fail

succeed : a -> Result a
succeed = Result [] << Just

log : Message -> Result a -> Result a
log msg (Result msgs val) = Result (msgs ++ [msg]) val

prependMsg : (List Message) -> Result a -> Result a
prependMsg premsgs (Result msgs val) = Result (premsgs ++ msgs) val

andThen : (a -> Result b) -> Result a -> Result b
andThen fn res =
    case res of
        Result msgs Nothing -> Result msgs Nothing
        Result msgs (Just val) ->
            let
                (Result newmsgs newval) = fn val
            in
                Result (msgs ++ newmsgs) newval

map : (a -> b) -> Result a -> Result b
map fn (Result msgs val) = Result msgs <| Maybe.map fn val

andMap : Result a -> Result (a -> b) -> Result b
andMap (Result msgsv val) (Result msgsf fn) = Result (msgsf ++ msgsv) (Maybe.Extra.andMap val fn)

map2 : (a -> b -> c) -> Result a -> Result b -> Result c
map2 fn a b = succeed fn |> andMap a |> andMap b

andThen3 : (a -> b -> c -> Result d) -> Result a -> Result b -> Result c -> Result d
andThen3 fn a b c =
    let
        (Result am av) = a
        (Result bm bv) = b
        (Result cm cv) = c
    in
        case (av, bv, cv) of
            (Just aa, Just bb, Just cc) -> fn aa bb cc |> prependMsg cm |> prependMsg bm |> prependMsg am
            _ -> Result (am ++ bm ++ cm) Nothing

foldr : (a -> b -> Result b) -> Result b -> Result (List a) -> Result b
foldr fn (Result initmsgs initial) (Result listmsgs list) =
    case (initial, list) of
        (Just initval, Just (listhead :: listtail)) ->
            let
                newInit = fn listhead initval |> prependMsg listmsgs |> prependMsg initmsgs
            in
                foldr fn newInit <| Result [] <| Just listtail
        (Just initval, Just []) -> (Result initmsgs initial)
        _ -> Result (initmsgs ++ listmsgs) Nothing

-- foldr fn (Result initmsgs initval) (Result listmsgs listval) =
--     case Maybe.map2 List.foldr initval listval of
--         Just (Result finalmsgs (Just finalval)) -> Result (initmsgs ++ listmsgs ++ finalmsgs) Just finalval
--         _ ->

withDefault : a -> Result a -> a
withDefault default result =
    case result of
        Result _ Nothing -> default
        Result _ (Just v) -> v

lift : String -> (a -> Maybe b) -> a -> Result b
lift message fn val =
    case fn val of
        Just v -> Result [] <| Just v
        Nothing -> Result [message] Nothing

-- lift : String -> Maybe a -> Result a
-- lift err val = Maybe.Extra.unwrap (fail err) succeed val

-- liftWarn : String -> Maybe a -> Result a
-- liftWarn err val = Maybe.Extra.unwrap (failWarn err) succeed val

modify : Lens a b -> (b -> Result b) -> a -> a
modify lens f init =
    let
        init1 = lens.get init
    in
        init1 |> f |> map (flip lens.set init) |> withDefault init

-- -- Not lazy!
-- ifThen : Result Bool -> Result a -> Result a -> Result a
-- ifThen flag thn els =
--     case flag of
--         R.Err err -> R.Err err
--         R.Ok True -> thn
--         R.Ok False -> els
