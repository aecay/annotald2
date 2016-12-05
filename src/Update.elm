module Update exposing (update, subscriptions)

import Model exposing (Model, root, selected, contextMenu)
import Selection
import ContextMenu
import Msg exposing (Msg(..))

import Utils exposing (do, maybeDo)

import Bindings exposing (bindings)

import Return exposing (Return, singleton)

import Dict

import Result as R

import Actions

import Keyboard
import Mouse

-- import ZipperExts as ZX

handleResult : Model -> Actions.Result -> Model
handleResult model result =
    case result of
        R.Ok m -> { m | lastMessage = "OK" }
        R.Err e -> case e of
                       Actions.Msg msg -> { model | lastMessage = msg }
                       Actions.Silent msg -> { model | lastMessage = "Failure in " ++ msg }

update : Msg -> Model -> Return Msg Model
update msg model =
    singleton model |>
    case msg of
        ToggleSelect z ->
            Return.map <| ContextMenu.hide contextMenu >>
                do selected (Selection.updateWith z)
        KeyMsg k ->
            Return.map <| \model ->
                Dict.get k bindings |>
                Actions.liftMaybe (Actions.Msg "Key is not bound") |>
                R.andThen (\x -> x model) |>
                handleResult model
        RightClick path position ->
            Return.map <|
                Selection.perform model.selected
                    (ContextMenu.show position path Model.contextMenu)
                    (\sel model -> Actions.doMove sel path model |> handleResult model)
                    (\_ _ -> Debug.crash "can't right click wtih two selected")
        Context contextMsg ->
            Return.map <| maybeDo root <| ContextMenu.update contextMsg
        ClickMsg -> Return.map <| ContextMenu.hide contextMenu

subscriptions : Model -> Sub Msg
subscriptions m = Sub.batch
                  [ Keyboard.presses KeyMsg
                  -- This is to implement closing of the context menu.  Most
                  -- clicks are handled through onClick handlers in the view
                  -- function.  TODO: make this a subscription of the
                  -- ContextMenu, not the global app
                  , Mouse.clicks (\_ -> ClickMsg)
                  ]
