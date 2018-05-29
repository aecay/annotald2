module TreeEdit.Update exposing (update, subscriptions)

-- Core libraries
import Dict
import Json.Decode as D
import Mouse

-- Third party libraries

import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as K
import Monocle.Lens as Lens
import RemoteData exposing (RemoteData(..))
import Return exposing (Return)
import Return.Optics exposing (refracto)
import ThirdParty.WindowEvents exposing (onWindow)
import UuidStream exposing (UuidStream)

-- Project libraries

import Route

import TreeEdit.Action as Action
import TreeEdit.Bindings exposing (bindings)
import TreeEdit.ContextMenu as ContextMenu
import TreeEdit.Dialog as Dialog
import TreeEdit.Metadata as Metadata
import TreeEdit.Metadata.Type as MetadataType
import TreeEdit.Model as Model exposing (root, selected)
import TreeEdit.Model.Type exposing (Model)
import TreeEdit.Msg as Msg exposing (Msg(..))
import TreeEdit.Path as Path
import TreeEdit.Ports as Ports
import TreeEdit.Result as R
import TreeEdit.Save as Save
import TreeEdit.Selection as Selection
import TreeEdit.Tree.Type as TreeType
import TreeEdit.Undo as Undo
import TreeEdit.Validate as Validate
import TreeEdit.View as View
import TreeEdit.View.LabelEdit as LabelEdit


editingMetadata : Model -> Bool
editingMetadata model = model.metadataForm |>
                        Maybe.map .fieldStates |>
                        Maybe.withDefault Dict.empty |>
                        Dict.values |>
                        List.any ((==) (MetadataType.Visible True))

editingLabel : Model -> Bool
editingLabel model =
    case model.labelForm of
        Just _ -> True
        Nothing -> False

update : Msg -> Model -> UuidStream String -> (Return Msg Model, UuidStream String)
update msg model uuids =
    let
        disableMouse = editingMetadata model || editingLabel model
        uuidsUntouched r = (r, uuids)
        singleton x = Return.singleton x |> uuidsUntouched
        newSelection = Tuple.mapFirst (Return.andThen <| flip Metadata.update MetadataType.NewSelection)
    in
        case msg of
            ToggleSelect z -> -- TODO: probably want to name this
                              -- something like "click"
                if disableMouse
                then singleton model
                else
                    singleton
                        ((ContextMenu.hide >> Lens.modify selected (Selection.updateWith z))
                             model) |>
                    newSelection
            KeyMsg {shiftKey, keyCode} ->
                let
                    key = Maybe.map (\x -> (if shiftKey then 1 else 0, x)) (K.code keyCode)
                    binding = Maybe.andThen (flip Dict.get bindings) key
                in
                    Maybe.map (\x -> x uuids model) binding |>
                    Maybe.withDefault (R.fail "key is not bound", uuids) |>
                    Tuple.mapFirst (R.handle model) |>
                    newSelection
            RightClick path position ->
                if disableMouse
                then singleton model
                else
                    (Selection.perform model.selected
                         (singleton <| ContextMenu.show position path model)
                         (\sel -> if path == sel
                                  then Return.singleton model |> -- Rightclick only selection -> show context menu
                                       Return.map (Lens.modify selected (Selection.updateWith sel)) |>
                                       Return.map (ContextMenu.show position path) |>
                                       uuidsUntouched
                                  else Action.doMove sel path uuids model |> Tuple.mapFirst (R.handle model))
                         (\_ _ -> singleton model)) -- TODO: support moving multiple nodes
            RightClickRoot ->
                if disableMouse
                then singleton model
                else
                    (Selection.perform model.selected
                         (singleton model)
                         (\sel -> Action.doMove sel Path.RootPath uuids model |> Tuple.mapFirst (R.handle model))
                         (\_ _ -> singleton model)) -- TODO: support moving multiple
                                                                             -- nodes
            Context contextMsg ->
                ContextMenu.update contextMsg model |> uuidsUntouched
            LoadedData (Success (trees, config, lemmata)) ->
                Return.return { model |
                                    webdata = Success { root = .ta TreeType.private "wtf" trees
                                                      , config = config
                                                      , viewFn = View.viewRootTree config
                                                      , lemmata = lemmata
                                                      }
                              }
                    (Ports.openFile model.fileName) |>
                    uuidsUntouched
            LoadedData x ->
                Debug.log ("fetch error: " ++ (toString x)) <| singleton model
            Save -> Save.perform model |> uuidsUntouched
            SaveFailure reason -> Save.failure model reason |> uuidsUntouched
            SaveSuccess -> Save.success model |> uuidsUntouched
            LogMessage m -> singleton { model | lastMessage = m }
            CancelContext -> singleton <| ContextMenu.hide model
            Metadata submsg ->
                let
                    (newmodel, subcmd) = Metadata.update model submsg
                in
                    Return.return newmodel subcmd |> uuidsUntouched
            Label submsg -> Return.singleton model |>
                            refracto Model.labelForm Msg.Label (LabelEdit.update submsg) |>
                            uuidsUntouched
            LabelKey {keyCode} -> case keyCode of
                                      K.Enter -> uuidsUntouched <| R.handle model <| Action.finishLabelEdit model
                                      K.Escape -> singleton { model | labelForm = Nothing }
                                      _ -> singleton model
            Copy (Success text) -> singleton { model | dialog = Just <| Dialog.Copy text }
            Copy _ -> singleton model
            DismissDialog -> singleton { model | dialog = Nothing }
            Validate -> Validate.perform model |> uuidsUntouched
            ValidateDone webdata -> Validate.done model webdata |> uuidsUntouched
            Undo -> Undo.undo model |> uuidsUntouched
            Redo -> Undo.redo model |> uuidsUntouched
            Dirty isDirty -> Return.return { model | dirty = isDirty } (Ports.dirty isDirty) |>
                             uuidsUntouched
            Exit -> if model.dirty
                    then singleton { model | lastMessage = "Cannot exit with unsaved changes" }
                    else Return.return model (Route.goTo Route.ListFiles) |> uuidsUntouched
            Ignore -> singleton model

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
        clickSub = if m.contextMenu == Nothing then Sub.none else (Mouse.clicks (\_ -> CancelContext))
    in
        Sub.batch [keySub, clickSub]
