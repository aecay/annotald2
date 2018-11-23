module TreeEdit.Result exposing
    ( Result(..)
    , andDo
    , andMap
    , andThen
    , andThen2
    , andThen3
    , do
    , fail
    , failWarn
    ,  guard
       -- , Failure(..)
       -- , foldr

    , handle
    , ifThen
    , lift
    , liftVal
    , map
    ,  modify
       -- , modifyO
    , succeed
    )


import Cmd.Extra exposing (perform)
import Maybe.Extra
import Monocle.Lens exposing (Lens)

import Return
import TreeEdit.Model as Model
import TreeEdit.Model.Type exposing (Model, ForestModel)
import TreeEdit.Msg exposing (LoadedMsg(..), Msg(..))
import TreeEdit.Utils exposing (uncurry3)


type alias Message =
    String


type Result a
    = Result (List Message) (List (Cmd Msg)) (Maybe a)


fail : String -> Result a
fail s =
    Result [ s ] [] Nothing


guard : Result Bool -> String -> Result a -> Result a
guard flag failMsg res =
    case flag of
        Result _ _ (Just True) ->
            joinSecond flag res

        Result msgs _ (Just False) ->
            toFail flag |> log failMsg

        Result _ _ Nothing ->
            toFail flag


joinSecond : Result a -> Result b -> Result b
joinSecond (Result msgs1 cmds1 _) (Result msgs2 cmds2 val) =
    Result (msgs1 ++ msgs2) (cmds1 ++ cmds2) val


toFail : Result a -> Result b
toFail (Result msgs cmds _) =
    Result msgs cmds Nothing



-- TODO: remove


failWarn : String -> Result a
failWarn =
    fail


succeed : a -> Result a
succeed =
    Result [] [] << Just


log : Message -> Result a -> Result a
log msg (Result msgs cmds val) =
    Result (msgs ++ [ msg ]) cmds val


prependMsg : List Message -> Result a -> Result a
prependMsg premsgs (Result msgs cmds val) =
    Result (premsgs ++ msgs) cmds val


andThen : (a -> Result b) -> Result a -> Result b
andThen fn res =
    case res of
        Result msgs cmds Nothing ->
            Result msgs cmds Nothing

        Result _ _ (Just val) ->
            joinSecond res (fn val)


andThen2 : (a -> b -> Result c) -> Result a -> Result b -> Result c
andThen2 fn a b =
    map Tuple.pair a |> andMap b |> andThen (\( x, y ) -> fn x y)


andThen3 : (a -> b -> c -> Result d) -> Result a -> Result b -> Result c -> Result d
andThen3 fn a b c =
    map (\x y z -> { x = x, y = y, z = z }) a |> andMap b |> andMap c |> andThen (\{x, y, z} -> fn x y z)


map : (a -> b) -> Result a -> Result b
map fn (Result msgs cmds val) =
    Result msgs cmds <| Maybe.map fn val


andMap : Result a -> Result (a -> b) -> Result b
andMap (Result msgsv cmdv val) (Result msgsf cmdf fn) =
    Result (msgsf ++ msgsv) (cmdf ++ cmdv) (Maybe.Extra.andMap val fn)



-- foldr : (a -> b -> Result b) -> Result b -> Result (List a) -> Result b
-- foldr fn (Result initmsgs initial) (Result listmsgs list) =
--     case (initial, list) of
--         (Just initval, Just (listhead :: listtail)) ->
--             let
--                 newInit = fn listhead initval |> prependMsg listmsgs |> prependMsg initmsgs
--             in
--                 foldr fn newInit <| Result [] <| Just listtail
--         (Just initval, Just []) -> (Result initmsgs initial)
--         _ -> Result (initmsgs ++ listmsgs) Nothing
-- foldr fn (Result initmsgs initval) (Result listmsgs listval) =
--     case Maybe.map2 List.foldr initval listval of
--         Just (Result finalmsgs (Just finalval)) -> Result (initmsgs ++ listmsgs ++ finalmsgs) Just finalval
--         _ ->


lift : String -> (a -> Maybe b) -> a -> Result b
lift message fn val =
    case fn val of
        Just v ->
            Result [] [] <| Just v

        Nothing ->
            Result [ message ] [] Nothing


liftVal : String -> Maybe a -> Result a
liftVal message =
    lift message identity



-- lift : String -> Maybe a -> Result a
-- lift err val = Maybe.Extra.unwrap (fail err) succeed val
-- liftWarn : String -> Maybe a -> Result a
-- liftWarn err val = Maybe.Extra.unwrap (failWarn err) succeed val


modify : Lens a b -> (b -> Result b) -> a -> Result a
modify lens f init =
    init |> lens.get |> f |> (map <| \a -> lens.set a init)



-- modifyO : Optional a b -> (b -> Result b) -> a -> a
-- modifyO lens f init =
--     let
--         init1 = lens.getOption init
--     in
--         case init1 of
--             Nothing -> init
--             Just val -> f val |> map (flip lens.set init) |> withDefault init
-- Not lazy!


ifThen : Result Bool -> Result a -> Result a -> Result a
ifThen flag thn els =
    case flag of
        Result _ _ (Just True) ->
            joinSecond flag thn

        Result _ _ (Just False) ->
            joinSecond flag els

        Result msgs cmds Nothing ->
            Result msgs cmds Nothing


do : Cmd Msg -> Result a -> Result a
do cmd (Result msgs cmds val) =
    Result msgs (cmds ++ [ cmd ]) val


andDo : (a -> Result (Cmd Msg)) -> Result a -> Result a
andDo fn result =
    case result of
        Result msgs cmds (Just val) ->
            let
                cmd =
                    fn val
            in
            case cmd of
                Result c_msgs c_cmds (Just c_val) ->
                    Result (msgs ++ c_msgs) (cmds ++ c_cmds ++ [ c_val ]) (Just val)

                Result _ _ Nothing ->
                    result

        Result _ _ Nothing ->
            result


handle : ForestModel -> Result ForestModel -> Return.Return Msg ForestModel
handle model result =
    case result of
        Result msgs cmds (Just newModel) ->
            let
                oldRoot = model.root
            in
                Return.return
                    { newModel |
                      undo = oldRoot :: model.undo
                    , redo = []
                    }
                (Cmd.batch <| perform (Dirty True) :: perform (LogMessage <| String.join "\n" msgs) :: cmds)

        Result msgs cmds Nothing ->
            -- TODO: cmds should always be empty in this case
            Return.return model (Cmd.batch <| perform (LogMessage <| String.join "\n" msgs) :: cmds)
