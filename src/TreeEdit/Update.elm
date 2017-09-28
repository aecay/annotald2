module TreeEdit.Update exposing (update, subscriptions)

import TreeEdit.Model as Model exposing (root, selected, contextMenu)
import TreeEdit.Model.Type exposing (Model)
import TreeEdit.Selection as Selection
import TreeEdit.ContextMenu as ContextMenu
import TreeEdit.Msg as Msg exposing (Msg(..))
import TreeEdit.Tree.Json exposing (toJson)
import TreeEdit.Tree as Tree exposing (children)
import TreeEdit.Metadata.Type as MetadataType
import TreeEdit.Metadata as Metadata
import TreeEdit.View as View
import TreeEdit.View.LabelEdit as LabelEdit

import TreeEdit.Utils as Utils

import TreeEdit.Bindings exposing (bindings)

import Return exposing (Return, singleton)
import Return.Optics exposing (refracto)
import Monocle.Lens as Lens
import RemoteData exposing (RemoteData(..))
import RemoteData.Http

import Dict
import Json.Decode as D
import Json.Encode as E

import TreeEdit.Result as R

import TreeEdit.Actions as Actions
import TreeEdit.Path as Path

import Keyboard
import Mouse

editingMetadata : Model -> Bool
editingMetadata model = model.metadataForm |>
                        Maybe.map Tuple.second |>
                        Maybe.withDefault Dict.empty |>
                        Dict.values |>
                        List.any ((==) MetadataType.Editing)

editingLabel : Model -> Bool
editingLabel model =
    case model.labelForm of
        Just _ -> True
        Nothing -> False

update : Msg -> Model -> Return Msg Model
update msg model =
    let
        disableMouse = editingMetadata model || editingLabel model
    in
        if False -- TODO: dummy to prevent reindenting everything yet
        then Return.singleton model
        else
            case msg of
                ToggleSelect z -> -- TODO: probably want to name this
                                  -- something like "click"
                    if disableMouse
                    then Return.singleton model
                    else
                        Return.singleton
                            ((ContextMenu.hide >> Lens.modify selected (Selection.updateWith z))
                                 model) |>
                        -- TODO: use return.optics here
                        Return.andThen (\x -> Metadata.update x MetadataType.NewSelection)
                KeyMsg k ->
                    let
                        binding = R.lift "Key is not bound" (Dict.get k) bindings
                    in
                        R.andThen (\x -> x model) binding |>
                        R.handle model |>
                        Return.andThen (\x -> Metadata.update x MetadataType.NewSelection)
                RightClick path position ->
                    if disableMouse
                    then Return.singleton model
                    else
                        (Selection.perform model.selected
                             (Return.singleton <| ContextMenu.show position path model)
                             (\sel -> Actions.doMove sel path model |> R.handle model)
                             (\_ _ -> Return.singleton model)) -- TODO: support moving multiple nodes
                RightClickRoot ->
                    if disableMouse
                    then Return.singleton model
                    else
                        (Selection.perform model.selected
                             (Return.singleton model)
                             (\sel -> Actions.doMove sel Path.RootPath model |> R.handle model)
                             (\_ _ -> Return.singleton model)) -- TODO: support moving multiple
                                                                     -- nodes
                Context contextMsg ->
                    ContextMenu.update contextMsg model
                LoadedData (Success (trees, config)) ->
                    Return.singleton { model |
                                       webdata = Success (Tree.t "wtf" trees, config)
                                     , viewRootWithConfig = Just <| View.viewRootTree config
                                     }
                LoadedData x ->
                    Debug.log ("fetch error: " ++ (toString x)) <| Return.singleton model
                DoSave ->
                    let
                        root = .get Model.root model
                        handle : (RemoteData.WebData () -> Msg)
                        handle d = case d of
                                       Success _ -> LogMessage "Save success"
                                       f -> LogMessage <| "Save failure: " ++ toString f
                    in
                        Return.return model <|
                            RemoteData.Http.post
                                "/save"
                                handle
                                (D.succeed ())
                                (E.object [ ("filename", E.string model.fileName)
                                          , ("trees", toJson <| Utils.fromJust <| .getOption children root)
                                          ])
                LogMessage m -> Return.singleton { model | lastMessage = m }
                CancelContext -> Return.singleton <| ContextMenu.hide model
                Metadata submsg ->
                    let
                        (newmodel, subcmd) = Metadata.update model submsg
                    in
                        Return.return newmodel subcmd
                Label submsg -> Return.singleton model |> refracto Model.labelForm Msg.Label (LabelEdit.update submsg)
                LabelKey code -> case code of
                                     13 -> R.handle model <| Actions.finishLabelEdit model
                                     27 -> Return.singleton { model | labelForm = Nothing }
                                     _ -> Return.singleton model
                Ignore -> Return.singleton model

subscriptions : Model -> Sub Msg
subscriptions m =
    let
        keySub = if editingMetadata m
                 then Keyboard.ups (Metadata << MetadataType.Key)
                 else
                     if m.labelForm == Nothing
                     then Keyboard.presses KeyMsg
                     else Keyboard.ups LabelKey
        clickSub = if m.contextMenu.target == Nothing then Sub.none else (Mouse.clicks (\_ -> CancelContext))
    in
        Sub.batch [keySub, clickSub]
