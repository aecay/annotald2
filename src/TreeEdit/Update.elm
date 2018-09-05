module TreeEdit.Update exposing (update, subscriptions)

-- Core libraries
import Array
import Dict
import Dom
import Json.Decode as D
import Mouse
import Task

-- Third party libraries

import ThirdParty.KeyboardEvent exposing (KeyboardEvent, decodeKeyboardEvent)
import ThirdParty.KeyboardKey as K
import Monocle.Lens as Lens
import Monocle.Optional exposing (Optional)
import RemoteData exposing (RemoteData(..))
import Return exposing (Return, ReturnF)
import ThirdParty.WindowEvents exposing (onWindow)

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
import TreeEdit.Ports as Ports
import TreeEdit.Result as R
import TreeEdit.Save as Save
import TreeEdit.Selection as Selection
import TreeEdit.Tree as Tree
import TreeEdit.Undo as Undo
import TreeEdit.Validate as Validate
import TreeEdit.View as View
import TreeEdit.View.LabelEdit as LabelEdit


-- Function from https://github.com/toastal/return-optics/tree/1.1.1
-- Original license: BSD
refracto : Optional pmod cmod -> (cmsg -> pmsg) -> (cmod -> Return cmsg cmod) -> ReturnF pmsg pmod
refracto opt mergeBack fx (( model, cmd ) as return) =
    opt.getOption model
        |> Maybe.map
            (fx
                >> Return.mapBoth mergeBack (flip opt.set model)
                >> Return.command cmd
            )
        |> Maybe.withDefault return

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
        singleton x = Return.singleton x
        newSelection = Return.andThen <| flip Metadata.update MetadataType.NewSelection
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
                    Maybe.map (\x -> x model) binding |>
                    Maybe.withDefault (R.fail "key is not bound") |>
                    R.handle model |>
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
                                       Return.map (ContextMenu.show position path)
                                  else Action.doMove sel path model |> R.handle model)
                         (\_ _ -> singleton model)) -- TODO: support moving multiple nodes
            RightClickRoot ->
                if disableMouse
                then singleton model
                else
                    (Selection.perform model.selected
                         (singleton model)
                         (\sel -> Action.doMoveToRoot sel model |> R.handle model)
                         (\_ _ -> singleton model)) -- TODO: support moving multiple
                                                                             -- nodes
            Context contextMsg ->
                ContextMenu.update contextMsg model
            LoadedData (Success (trees, config, lemmata)) ->
                Return.return { model |
                                    webdata = Success { root = Tree.forestFromList <| Array.toList trees
                                                      , config = config
                                                      , viewFn = View.viewRootTree config
                                                      , lemmata = lemmata
                                                      }
                              }
                    (Ports.openFile model.fileName)
            LoadedData x ->
                Debug.log ("fetch error: " ++ (toString x)) <| singleton model
            Save -> Save.perform model
            SaveFailure reason -> Save.failure model reason
            SaveSuccess -> Save.success model
            LogMessage m -> singleton { model | lastMessage = m }
            CancelContext -> singleton <| ContextMenu.hide model
            Metadata submsg ->
                let
                    (newmodel, subcmd) = Metadata.update model submsg
                in
                    Return.return newmodel subcmd
            Label submsg -> Return.singleton model |>
                            refracto Model.labelForm Msg.Label (LabelEdit.update submsg)
            LabelKey {keyCode} -> case keyCode of
                                      K.Enter -> R.handle model <| Action.finishLabelEdit model
                                      K.Escape -> singleton { model | labelForm = Nothing }
                                      _ -> singleton model
            Copy (Success text) -> singleton { model | dialog = Just <| Dialog.Copy text }
            Copy _ -> singleton model
            DismissDialog -> singleton { model | dialog = Nothing }
            Validate -> Validate.perform model
            ValidateDone idx webdata -> Validate.done model idx webdata
            Undo -> Undo.undo model
            Redo -> Undo.redo model
            Dirty isDirty -> Return.return { model | dirty = isDirty } (Ports.dirty isDirty)
            Blur id -> Return.return model (Task.attempt (\_ -> Ignore) <| Dom.blur id)
            Exit -> if model.dirty
                    then singleton { model | lastMessage = "Cannot exit with unsaved changes" }
                    else Return.return model (Cmd.batch [ Route.goTo Route.ListFiles
                                                        , Ports.saveScroll ()
                                                        ])
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
