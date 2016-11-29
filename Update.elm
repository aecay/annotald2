module Update exposing (update, Msg(..), subscriptions)

import Model exposing (Model)
import Tree exposing (Tree, TreeZipper)
import Selection

import Bindings exposing (bindings)

import Return exposing (Return, singleton)

import Dict

import Keyboard

-- import ZipperExts as ZX

type Msg = ToggleSelect TreeZipper |
    KeyMsg Keyboard.KeyCode

update : Msg -> Model -> Return Msg Model
update msg model =
    let
        update : Model -> Model
        update =
            case msg of
                ToggleSelect z ->
                    \m -> { m | selected = Selection.updateWith z m.selected }
                KeyMsg k ->
                    Dict.get k bindings |>
                    (\y m ->
                         y |>
                         Maybe.andThen (\x -> x m) |>
                         Maybe.withDefault m)
    in
        singleton model |>
        Return.map update

subscriptions : Model -> Sub Msg
subscriptions m = Sub.batch
                  [ Keyboard.presses KeyMsg
                  ]
