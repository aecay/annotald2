module TreeEdit.Update exposing (update, subscriptions)

import TreeEdit.Model as Model exposing (Model, root, selected, contextMenu)
import TreeEdit.Selection as Selection
import TreeEdit.ContextMenu as ContextMenu
import TreeEdit.Msg as Msg exposing (Msg(..))
import TreeEdit.Tree.Json exposing (toJson)
import TreeEdit.Tree exposing (children)

import TreeEdit.Utils as Utils

import TreeEdit.Bindings exposing (bindings)

import Return exposing (Return, singleton)
import Monocle.Lens as Lens
import RemoteData exposing (RemoteData(..))
import RemoteData.Http

import Dict
import Json.Decode as D
import Json.Encode as E
import Cmd.Extra

import Result as R

import TreeEdit.Res as Res

import TreeEdit.Actions as Actions
import TreeEdit.Path as Path

import Keyboard
import Mouse

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
        DoSave ->
            let
                handle : (RemoteData.WebData () -> Msg)
                handle d = case d of
                               Success _ -> LogMessage "Save success"
                               f -> LogMessage <| "Save failure: " ++ toString f
            in
                Return.command <|
                    case model.root of
                        Success tree ->
                            RemoteData.Http.post
                                "/save"
                                handle
                                (D.succeed ())
                                (E.object [ ("filename", E.string model.fileName)
                                          , ("trees", toJson <| Utils.fromJust <| .getOption children tree)
                                          ])

                        _ -> Cmd.Extra.perform <| LogMessage "Trying to save unloaded trees"
        LogMessage m ->
            Return.map (\x -> { x | message = m })
        CancelContext -> Return.map <| ContextMenu.hide contextMenu

subscriptions : Model -> Sub Msg
subscriptions m = Sub.batch <|
                  [ Keyboard.presses KeyMsg
                  ] ++ if m.contextMenu.target == Nothing
                       then []
                       else [ Mouse.clicks (\_ -> CancelContext) ]
