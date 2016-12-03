module Update exposing (update, Msg(..), subscriptions)

import Model exposing (Model)
import Tree exposing (Tree, Path)
import Selection

import Bindings exposing (bindings)

import Return exposing (Return, singleton)

import Dict

import Keyboard

import Result as R

import Actions

-- import ZipperExts as ZX

type Msg = ToggleSelect Path |
    KeyMsg Keyboard.KeyCode |
    RightClick Path

handleResult : Model -> Actions.Result -> Model
handleResult model result =
    case result of
        R.Ok m -> { m | lastMessage = "OK" }
        R.Err e -> case e of
                       Actions.Msg msg -> { model | lastMessage = msg }
                       Actions.Silent msg -> { model | lastMessage = "Failure in " ++ msg }

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
                         Actions.liftMaybe (Actions.Msg "Key is not bound") |>
                         R.andThen (\x -> x m) |>
                         handleResult m)
                RightClick p ->
                    -- TODO: context menu
                    \m -> Actions.doMove p m |> handleResult m
    in
        singleton model |>
        Return.map update

subscriptions : Model -> Sub Msg
subscriptions m = Sub.batch
                  [ Keyboard.presses KeyMsg
                  ]
