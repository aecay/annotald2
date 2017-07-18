module TreeEdit.Update exposing (update, subscriptions)

import TreeEdit.Model as Model exposing (Model, root, selected, contextMenu)
import TreeEdit.Selection as Selection
import TreeEdit.ContextMenu as ContextMenu
import TreeEdit.Msg as Msg exposing (Msg(..))

-- import Utils exposing (do, maybeDo)

import TreeEdit.Bindings exposing (bindings)

import Return exposing (Return, singleton)
import Monocle.Lens as Lens
import RemoteData exposing (RemoteData(..))

import Dict

import Result as R

import TreeEdit.Res as Res

import TreeEdit.Actions as Actions
import TreeEdit.Path as Path

import Keyboard

-- import ZipperExts as ZX

handleResult : Model -> Res.Result Model -> Model
handleResult model result =
    case result of
        R.Ok m -> { m | lastMessage = "OK" }
        R.Err e -> case e of
                       Res.Fail msg -> { model | lastMessage = msg }
                       Res.Warn msg -> { model | lastMessage = "Failure in " ++ msg }

update : Msg -> Model -> Return Msg Model
update msg model =
    singleton model |>
    case msg of
        ToggleSelect z ->
            Return.map <| ContextMenu.hide contextMenu >>
                Lens.modify selected (Selection.updateWith z)
        KeyMsg k ->
            Return.map <| \model ->
                Dict.get k bindings |>
                Res.liftWarn "Key is not bound" |>
                R.andThen (\x -> x model) |>
                handleResult model
        RightClick path position ->
            Return.map <|
                Selection.perform model.selected
                    (ContextMenu.show position path Model.contextMenu)
                    (\sel model -> Actions.doMove sel path model |> handleResult model)
                    (\_ _ model -> model) -- TODO: support moving multiple nodes
        RightClickRoot ->
            Return.map <|
                Selection.perform model.selected
                    (\model -> model)
                    (\sel model -> Actions.doMove sel Path.RootPath model |> handleResult model)
                    (\_ _ model -> model) -- TODO: support moving multiple nodes
        Context contextMsg ->
            Return.map <| ContextMenu.update contextMsg Model.root Model.contextMenu
        GotTrees (Success trees) ->
            Return.map (\x -> Model.withTrees trees x.fileName)
        GotTrees x ->
            Debug.log ("fetch error: " ++ (toString x))

subscriptions : Model -> Sub Msg
subscriptions m = Sub.batch
                  [ Keyboard.presses KeyMsg
                  ]
