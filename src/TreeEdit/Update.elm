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
import Return exposing (Return, singleton)
import Return.Optics exposing (refracto)
import ThirdParty.WindowEvents exposing (onWindow)

-- Project libraries

import TreeEdit.Action as Action
import TreeEdit.Bindings exposing (bindings)
import TreeEdit.ContextMenu as ContextMenu
import TreeEdit.Dialog as Dialog
import TreeEdit.Metadata as Metadata
import TreeEdit.Metadata.Type as MetadataType
import TreeEdit.Model as Model exposing (root, selected, contextMenu)
import TreeEdit.Model.Type exposing (Model)
import TreeEdit.Msg as Msg exposing (Msg(..))
import TreeEdit.Path as Path
import TreeEdit.Ports as Ports
import TreeEdit.Result as R
import TreeEdit.Save as Save
import TreeEdit.Selection as Selection
import TreeEdit.Tree as Tree exposing (children)
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

update : Msg -> Model -> Return Msg Model
update msg model =
    let
        disableMouse = editingMetadata model || editingLabel model
    in
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
                                  else Action.doMove sel path model |> R.handle model)
                         (\_ _ -> Return.singleton model)) -- TODO: support moving multiple nodes
            RightClickRoot ->
                if disableMouse
                then Return.singleton model
                else
                    (Selection.perform model.selected
                         (Return.singleton model)
                         (\sel -> Action.doMove sel Path.RootPath model |> R.handle model)
                         (\_ _ -> Return.singleton model)) -- TODO: support moving multiple
                                                                 -- nodes
            Context contextMsg ->
                ContextMenu.update contextMsg model
            LoadedData (Success (trees, config, lemmata)) ->
                Return.return { model |
                                    webdata = Success { root = Tree.t "wtf" trees
                                                      , config = config
                                                      , viewFn = View.viewRootTree config
                                                      , lemmata = lemmata
                                                      }
                              }
                    (Ports.openFile model.fileName)
            LoadedData x ->
                Debug.log ("fetch error: " ++ (toString x)) <| Return.singleton model
            Save -> Save.perform model
            SaveFailure reason -> Save.failure model reason
            SaveSuccess -> Save.success model
            LogMessage m -> Return.singleton { model | lastMessage = m }
            CancelContext -> Return.singleton <| ContextMenu.hide model
            Metadata submsg ->
                let
                    (newmodel, subcmd) = Metadata.update model submsg
                in
                    Return.return newmodel subcmd
            Label submsg -> Return.singleton model |> refracto Model.labelForm Msg.Label (LabelEdit.update submsg)
            LabelKey {keyCode} -> case keyCode of
                                      K.Enter -> R.handle model <| Action.finishLabelEdit model
                                      K.Escape -> Return.singleton { model | labelForm = Nothing }
                                      _ -> Return.singleton model
            Copy (Success text) -> Return.singleton { model | dialog = Just <| Dialog.Copy text }
            Copy _ -> Return.singleton model
            DismissDialog -> Return.singleton { model | dialog = Nothing }
            Validate -> Validate.perform model
            ValidateDone webdata -> Validate.done model webdata
            Undo -> Undo.undo model
            Redo -> Undo.redo model
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
