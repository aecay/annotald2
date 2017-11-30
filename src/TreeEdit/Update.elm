module TreeEdit.Update exposing (update, subscriptions)

-- Core libraries
import Dict
import Json.Decode as D
import Json.Encode as E
import Mouse

-- Third party libraries

import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as K
import Monocle.Lens as Lens
import RemoteData exposing (RemoteData(..))
import RemoteData.Http
import Return exposing (Return, singleton)
import Return.Optics exposing (refracto)
import ThirdParty.WindowEvents exposing (onWindow)

-- Project libraries

import TreeEdit.Actions as Actions
import TreeEdit.Bindings exposing (bindings)
import TreeEdit.ContextMenu as ContextMenu
import TreeEdit.Dialog as Dialog
import TreeEdit.Metadata as Metadata
import TreeEdit.Metadata.Type as MetadataType
import TreeEdit.Model as Model exposing (root, selected, contextMenu)
import TreeEdit.Model.Type exposing (Model)
import TreeEdit.Msg as Msg exposing (Msg(..))
import TreeEdit.Path as Path
import TreeEdit.Result as R
import TreeEdit.Selection as Selection
import TreeEdit.Tree as Tree exposing (children)
import TreeEdit.Tree.Json exposing (toJson)
import TreeEdit.Utils as Utils
import TreeEdit.View as View
import TreeEdit.View.LabelEdit as LabelEdit


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
                KeyMsg {shiftKey, keyCode} ->
                    let
                        key = Maybe.map (\x -> (if shiftKey then 1 else 0, x)) (K.code keyCode)
                        binding = Maybe.andThen (flip Dict.get bindings) key |>
                                  R.liftVal "Key is not bound"
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
                             (\sel -> if path == sel
                                      then Return.singleton model |> -- Rightclick only selection -> show context menu
                                           Return.map (Lens.modify selected (Selection.updateWith sel)) |>
                                           Return.map (ContextMenu.show position path)
                                      else Actions.doMove sel path model |> R.handle model)
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
                LabelKey {keyCode} -> case keyCode of
                                          K.Enter -> R.handle model <| Actions.finishLabelEdit model
                                          K.Escape -> Return.singleton { model | labelForm = Nothing }
                                          _ -> Return.singleton model
                Ignore -> Return.singleton model

subscriptions : Model -> Sub Msg
subscriptions m =
    let
        keySubMsg = if editingMetadata m
                    then Metadata << MetadataType.Key
                    else
                        if m.labelForm == Nothing
                        then KeyMsg
                        else LabelKey
        keySub = onWindow "keyup" (D.map keySubMsg decodeKeyboardEvent)
        clickSub = if m.contextMenu.target == Nothing then Sub.none else (Mouse.clicks (\_ -> CancelContext))
    in
        Sub.batch [keySub, clickSub]
