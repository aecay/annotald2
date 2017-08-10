module TreeEdit.Update exposing (update, subscriptions)

import TreeEdit.Model as Model exposing (Model, root, selected, contextMenu)
import TreeEdit.Selection as Selection
import TreeEdit.ContextMenu as ContextMenu
import TreeEdit.Msg as Msg exposing (Msg(..))
import TreeEdit.Tree.Json exposing (toJson)
import TreeEdit.Tree exposing (children)
import TreeEdit.Metadata.Type as MetadataType
import TreeEdit.Metadata as Metadata


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

import TreeEdit.Result as R

import TreeEdit.Actions as Actions
import TreeEdit.Path as Path

import Keyboard
import Mouse

handleResult : Model -> R.Result Model -> Model
handleResult model result =
    case result of
        R.Result msgs (Just newModel) -> { newModel | lastMessage = String.join "\n" msgs }
        R.Result msgs Nothing -> { model | lastMessage = String.join "\n" msgs }

editingMetadata : Model -> Bool
editingMetadata model = model.metadataForm |>
                        Maybe.map Tuple.second |>
                        Maybe.withDefault Dict.empty |>
                        Dict.values |>
                        List.any identity

update : Msg -> Model -> Return Msg Model
update msg model =
    let
        handleMetadata submsg = case model.root of
                                    Success root ->
                                        let
                                            (newmodel, subcmd) = Metadata.update model submsg
                                        in
                                            Return.return newmodel <| Cmd.map Metadata subcmd
                                    _ -> Debug.crash "Got a metadata Cmd when no data was loaded"
    in
        if editingMetadata model
        then
            case msg of
                Metadata submsg -> handleMetadata submsg
                KeyMsg code -> case code of
                                   27 -> handleMetadata MetadataType.Cancel -- Escape
                                   _ -> Return.singleton model
                _ -> Return.singleton <|
                     handleResult model <|
                     R.fail "Can't do that while editing metadata"
        else
            case msg of
                ToggleSelect z ->
                    Return.singleton
                        ((ContextMenu.hide contextMenu >> Lens.modify selected (Selection.updateWith z))
                             model) |>
                    Return.andThen (\x -> Metadata.update x MetadataType.NewSelection ) |>
                    Return.mapCmd Msg.Metadata
                KeyMsg k ->
                    let
                        binding = R.lift "Key is not bound" (Dict.get k) bindings
                    in
                        Return.singleton <| handleResult model <| R.andThen (\x -> x model) binding
                    -- TODO: selecton may have changed
                RightClick path position ->
                    Return.singleton <|
                        (Selection.perform model.selected
                             (ContextMenu.show position path Model.contextMenu)
                             (\sel model -> Actions.doMove sel path model |> handleResult model)
                             (\_ _ model -> model)) -- TODO: support moving multiple
                                               -- nodes
                        model
                RightClickRoot ->
                    Return.singleton <|
                        (Selection.perform model.selected
                             (\model -> model)
                             (\sel model -> Actions.doMove sel Path.RootPath model |> handleResult model)
                             (\_ _ model -> model)) -- TODO: support moving multiple
                                               -- nodes
                        model
                Context contextMsg ->
                    Return.singleton <| (ContextMenu.update contextMsg Model.root Model.contextMenu) model
                GotTrees (Success trees) ->
                    Return.singleton <| Model.withTrees trees model.fileName
                GotTrees x ->
                    Debug.log ("fetch error: " ++ (toString x)) <| Return.singleton model
                DoSave ->
                    let
                        handle : (RemoteData.WebData () -> Msg)
                        handle d = case d of
                                       Success _ -> LogMessage "Save success"
                                       f -> LogMessage <| "Save failure: " ++ toString f
                    in
                        Return.return model <|
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
                LogMessage m -> Return.singleton { model | lastMessage = m }
                CancelContext -> Return.singleton <| ContextMenu.hide contextMenu <| model
                Metadata submsg -> handleMetadata submsg

subscriptions : Model -> Sub Msg
subscriptions m =
    let
        keySub = if editingMetadata m then Keyboard.downs KeyMsg else Keyboard.presses KeyMsg
        clickSub = if m.contextMenu.target == Nothing then Sub.none else (Mouse.clicks (\_ -> CancelContext))
    in
        Sub.batch [keySub, clickSub]
