-- Source: https://github.com/Gizra/elm-keyboard-event
-- License: MIT


module ThirdParty.KeyboardEvent exposing (KeyboardEvent, decodeKeyboardEvent)

import Json.Decode exposing (Decoder, andThen, bool, fail, field, int, map, map7, maybe, oneOf, string, succeed)
import String
import ThirdParty.KeyboardKey exposing (Key, fromCode)


type alias KeyCode =
    Int


type alias KeyboardEvent =
    { altKey : Bool
    , ctrlKey : Bool
    , key : Maybe String
    , keyCode : Key
    , metaKey : Bool
    , repeat : Bool
    , shiftKey : Bool
    }


decodeKeyboardEvent : Decoder KeyboardEvent
decodeKeyboardEvent =
    map7 KeyboardEvent
        (field "altKey" bool)
        (field "ctrlKey" bool)
        decodeKey
        (map fromCode decodeKeyCode)
        (field "metaKey" bool)
        (field "repeat" bool)
        (field "shiftKey" bool)


decodeKey : Decoder (Maybe String)
decodeKey =
    field "key" string
        |> andThen
            (\key ->
                if String.isEmpty key then
                    fail "empty key"

                else
                    succeed key
            )
        |> maybe


decodeKeyCode : Decoder KeyCode
decodeKeyCode =
    oneOf
        [ field "keyCode" decodeNonZero
        , field "which" decodeNonZero
        , field "charCode" decodeNonZero

        -- In principle, we should always get some code, so instead
        -- of making this a Maybe, we succeed with 0.
        , succeed 0
        ]


decodeNonZero : Decoder Int
decodeNonZero =
    andThen
        (\code ->
            if code == 0 then
                fail "code was zero"

            else
                succeed code
        )
        int
