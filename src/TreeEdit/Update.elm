module TreeEdit.Update exposing (subscriptions, update)

import Array
import Browser.Dom
import Browser.Events
import Dict
import Json.Decode as D
import Route
import Task

import Cmd.Extra
import Monocle.Lens as Lens
import Monocle.Optional exposing (Optional)
import RemoteData exposing (RemoteData(..))
import Return exposing (Return, ReturnF)

import ThirdParty.KeyboardEvent exposing (KeyboardEvent, decodeKeyboardEvent)
import ThirdParty.KeyboardKey as K

import TreeEdit.Action as Action
import TreeEdit.Bindings as Bindings
import TreeEdit.ContextMenu as ContextMenu
import TreeEdit.Dialog.Type as Dialog
import TreeEdit.Metadata as Metadata
import TreeEdit.Metadata.Type as MetadataType
import TreeEdit.Metadata.Util exposing (makeLemmata)
import TreeEdit.Model as Model
import TreeEdit.Model.Type exposing (Model, ForestModel)
import TreeEdit.Msg as Msg exposing (Msg(..), LoadedMsg(..))
import TreeEdit.Ports as Ports
import TreeEdit.Result as R
import TreeEdit.Save as Save
import TreeEdit.Selection as Selection
import TreeEdit.Tree as Tree
import TreeEdit.Undo as Undo
import TreeEdit.Validate as Validate
import TreeEdit.View as View
import TreeEdit.View.LabelEdit.Type as LabelEditType
import TreeEdit.View.LabelEdit as LabelEdit


-- Function from https://github.com/toastal/return-optics/tree/1.1.1
-- Original license: BSD
refracto : Optional pmod cmod -> (cmsg -> pmsg) -> (cmod -> Return cmsg cmod) -> ReturnF pmsg pmod
refracto opt mergeBack fx (( model, cmd ) as return) =
    opt.getOption model
        |> Maybe.map
            (fx
                >> Return.mapBoth mergeBack (\a -> opt.set a model)
                >> Return.command cmd
            )
        |> Maybe.withDefault return


editingMetadata : ForestModel -> Bool
editingMetadata model =
    model.metadataForm
        |> Maybe.map .fieldStates
        |> Maybe.withDefault Dict.empty
        |> Dict.values
        |> List.any ((==) (MetadataType.Visible True))


editingLabel : ForestModel -> Bool
editingLabel model =
    case model.labelForm of
        Just _ ->
            True

        Nothing ->
            False

updateForest : LoadedMsg -> ForestModel -> Return Msg ForestModel
updateForest msg model =
    let
        disableMouse = editingMetadata model || editingLabel model
        newSelection = Return.andThen <| \a -> Metadata.update a MetadataType.NewSelection
    in
    case msg of
        ToggleSelect z ->
            -- TODO: probably want to name this
            -- something like "click"
            if disableMouse then
                Return.singleton model
            else
                model
                  |> ContextMenu.hide
                  |> (\x -> { x | selected = Selection.updateWith z x.selected })
                  |> Return.singleton
                  |> newSelection

        KeyMsg { shiftKey, keyCode } ->
            case keyCode of
                K.Z ->
                    if shiftKey
                    then Undo.redo model
                          |> Return.map (\x -> { x | selected = Selection.empty})
                          |> newSelection
                    else Undo.undo model
                          |> Return.map (\x -> { x | selected = Selection.empty})
                          |> newSelection
                _ ->
                    (if shiftKey then 1 else 0, keyCode)
                      |> Bindings.get
                      |> Maybe.map (\x -> x model)
                      |> Maybe.withDefault (R.fail "key is not bound")
                      |> R.handle model
                      |> newSelection

        RightClick path position ->
            if disableMouse then
                Return.singleton model
            else
                Selection.perform model.selected
                    (Return.singleton <| ContextMenu.show position path model)
                    (\sel ->
                         if path == sel
                         then
                             Return.singleton model
                               |> Return.map (\x -> { x | selected = Selection.updateWith sel x.selected })
                               -- Rightclick on the only selection -> show context menu
                               |> Return.map (ContextMenu.show position path)
                         else
                             Action.doMove sel path model |> R.handle model
                    )
                    (\a b -> Action.move2 path a b model |> R.handle model)

        -- TODO: support moving multiple nodes
        RightClickRoot ->
            if disableMouse then
                Return.singleton model
            else
                Selection.perform model.selected
                    (Return.singleton model)
                    (\sel -> Action.doMoveToRoot sel model |> R.handle model)
                    (\_ _ -> Return.singleton model)

        Context contextMsg ->
            ContextMenu.update contextMsg model

        CancelContext ->
            Return.singleton <| ContextMenu.hide model

        Metadata submsg ->
            let
                ( newmodel, subcmd ) = Metadata.update model submsg
            in
                Return.return newmodel subcmd

        Label submsg ->
            case model.labelForm of
                Just lf ->
                    let
                        newForm : Return LabelEditType.Msg (Maybe LabelEditType.LabelForm)
                        newForm = LabelEdit.update submsg lf |> Return.map Just
                    in
                        Return.mapBoth (Loaded << Label) (\x -> { model | labelForm = x }) newForm
                Nothing ->
                    Debug.log "received a Label msg when no label form was active" submsg |>
                    (always <| Return.singleton model)

        LabelKey { keyCode } ->
            case keyCode of
                K.Enter ->
                    R.handle model <| Action.finishLabelEdit model
                K.Escape ->
                    Return.singleton { model | labelForm = Nothing }
                _ ->
                    Return.singleton model

        Copy (Success text) ->
            Return.return model <| Cmd.Extra.perform <| SetDialog <| Just <| Dialog.Copy text

        Copy _ ->
            Return.singleton model

        Validate ->
            Validate.perform model

        ValidateDone idx webdata ->
            Validate.done model idx webdata

        Undo ->
            Undo.undo model


        Redo ->
            Undo.redo model
              |> Return.map (\x -> { x | selected = Selection.empty})
              |> newSelection

update : (String -> Cmd Msg) -> Msg -> Model -> Return Msg Model
update goto msg model =
    case msg of
        LoadedData x ->
            case x of
                NotAsked -> Return.singleton { model | webdata = NotAsked }
                Loading -> Return.singleton { model | webdata = Loading }
                Failure e -> Return.singleton { model | webdata = Failure e }
                Success ( trees, config, lemmata ) ->
                    let
                        forest = Tree.forestFromList <| Array.toList trees
                    in
                    Return.return
                        { model
                            | webdata =
                                Success
                                    { root = forest
                                    , config = config
                                    , lemmata = makeLemmata lemmata forest
                                    , selected = Selection.empty
                                    , contextMenu = Nothing
                                    , metadataForm = Nothing
                                    , labelForm = Nothing
                                    , undo = []
                                    , redo = []
                                    , seed = model.seed
                                    }
                        }
                        (Ports.openFile model.fileName)

        LogMessage m ->
            Return.singleton { model | lastMessage = m }

        Blur id ->
            Return.return model (Task.attempt (\_ -> Ignore) <| Browser.Dom.blur id)

        Ignore ->
            Return.singleton model

        Loaded loadedMsg ->
            case model.webdata of
                Success forestModel ->
                    let
                        ret = updateForest loadedMsg forestModel
                    in
                        ret
                          |> Return.map (\x -> { model |
                                                 webdata = Success x
                                               , seed = x.seed
                                               })
                _ -> Debug.log "got a LoadedMsg while no data was loaded" <| Return.singleton model

        SetDialog d ->
            Return.singleton { model | dialog = d }

        Save ->
            case model.webdata of
                Success forestModel ->
                    Save.perform model.fileName forestModel |>
                      Return.map (always model) -- TODO: kinda ugly....
                _ ->
                    Debug.log "Got command to save when no data was loaded" <| Return.singleton model

        SaveFailure reason ->
            Save.failure model reason

        SaveSuccess ->
            Save.success model

        Dirty isDirty ->
            Return.return { model | dirty = isDirty } (Ports.dirty isDirty)

        Exit ->
            if model.dirty then
                Return.singleton { model | lastMessage = "Cannot exit with unsaved changes" }
            else
                Return.return model <| Ports.saveScroll True
        ScrollSaved ->
            Return.return model <| goto <| Route.toString Route.ListFiles



subscriptions : Model -> Sub Msg
subscriptions m =
    case m.webdata of
        Success submodel ->
            let
                keySubMsg =
                    if editingMetadata submodel
                    then Msg.Loaded << Msg.Metadata << MetadataType.Key
                    else
                        if submodel.labelForm == Nothing
                        then Msg.Loaded << Msg.KeyMsg
                        else Msg.Loaded << Msg.LabelKey

                keySub =
                    Browser.Events.onKeyUp (D.map keySubMsg decodeKeyboardEvent)

                clickSub =
                    if submodel.contextMenu == Nothing
                    then Sub.none
                    else Browser.Events.onClick (D.succeed <| Loaded <| CancelContext)
            in
                Sub.batch [ keySub, clickSub, Ports.scrollSaved (\_ -> ScrollSaved) ]
        _ -> Sub.none
