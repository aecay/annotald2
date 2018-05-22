module TreeEdit.Metadata exposing (view, update)

import Dict exposing (Dict)
import Html as Html exposing (div, text, button, Html, span, ul, li)
import Html.Attributes as Attr exposing (id)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Keyboard.Key as K
import Maybe.Extra as MX
import Monocle.Lens as Lens
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Net
import Return exposing (Return)

import Form exposing (Form)
import Form.Init
import Form.Field
import Form.Validate as V exposing (Validation)

import Maybe.Extra
import OrderedDict as OD
import Select

import TreeEdit.Metadata.Type exposing (..)
import TreeEdit.Metadata.Css as Css
import TreeEdit.Metadata.Util exposing (..)
import TreeEdit.Selection exposing (Selection)
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Model as Model
import TreeEdit.Model.Type as BigModel
import TreeEdit.Ports
import TreeEdit.Selection as Selection
import TreeEdit.Result as R
import TreeEdit.View.Theme exposing (theme)
import TreeEdit.Msg as Msg
import TreeEdit.Utils exposing (fromJust, message)

type alias Updater = String -> Maybe String -> Tree -> Return Msg.Msg Tree

fnUpdater : (String -> String) -> Updater
fnUpdater fn key newVal sel =
    sel |>
    Lens.modify Tree.metadata (Dict.update key (always <| Maybe.map fn newVal)) |>
    Return.singleton

genericUpdater : Updater
genericUpdater =
    fnUpdater identity

definitionUpdater : Updater
definitionUpdater _ newDef sel =
    let
        l = (.get Tree.metadata) sel |> Dict.get "LEMMA"
        updateCmd def lemma = Net.post
                              "/dictentry"
                              (always <| Msg.Metadata <| SaveSuccess lemma)
                              (D.succeed ())
                              (E.object [ ("lemma", E.string lemma)
                                        , ("definition", E.string def)
                                        ])
    in
        Maybe.map2 updateCmd newDef l |>
        Maybe.withDefault Cmd.none |>
        Return.return sel

type alias FieldInfo = { predicate : Tree -> Bool
                       , formatter : Formatter
                       , editInfo : Maybe (EditWidget, Updater)
                       }

lemma      : FieldInfo
lemma      = FieldInfo Tree.isTerminal                 formatters.value      <| Just (widgets.textbox,
                                                                                          -- The widget will actually
                                                                                          -- get swapped out below as
                                                                                          -- a special case,
                                                                                          -- since it needs to take the
                                                                                          -- model as an argument
                                                                                          genericUpdater)
definition : FieldInfo
definition = FieldInfo (hasMetadata "LEMMA")           formatters.definition <| Just (widgets.textbox, definitionUpdater)
gender     : FieldInfo
gender     = FieldInfo isNominal                       formatters.value      Nothing
number     : FieldInfo
number     = FieldInfo (\x -> isNominal x || isVerb x) formatters.value      Nothing
origTag    : FieldInfo
origTag    = FieldInfo Tree.isTerminal                 formatters.value      <| Just (widgets.textbox, fnUpdater String.toUpper)

mood : FieldInfo
mood =
    let
        moods = ["ind", "konj"]
    in
        FieldInfo isVerb formatters.value <| Just (widgets.options moods, genericUpdater)

case_ : FieldInfo
case_ =
    let
        caseOptions = ["nom", "gen", "dat", "akk"]
        pred = eitherP isNominal isPreposition
    in
        FieldInfo pred formatters.value <| Just (widgets.options caseOptions, genericUpdater)

fieldInfo : OD.OrderedDict String FieldInfo
fieldInfo = OD.fromList
            [ ("lemma", lemma)
            , ("definition", definition)
            , ("orig-tag", origTag)
            , ("case", case_)
            , ("gender", gender)
            , ("number", number)
            , ("mood", mood)
            , ("validation-error", FieldInfo (hasMetadata "VALIDATION-ERROR") formatters.validationError Nothing)
            ]

validation : Validation () Metadata
validation =
    let
        update str dict val = Dict.update str (always <| val) dict
        do : String -> Validation () Metadata -> Validation () Metadata
        do str val = val |>
                     V.andThen (\dict -> V.field str (V.string |> V.maybe |> V.map (update str dict)))
        init = V.succeed Dict.empty
    in
        List.foldl do init (OD.keys fieldInfo)

field : Model -> String -> Html Msg
field model name =
    let
        state = Dict.get name model.fieldStates |> Maybe.withDefault Hidden
        contents = Form.getFieldAsString name model.form
        ({editInfo} as infoPre) = OD.get name fieldInfo |> fromJust
        info = if name == "lemma"
               then { infoPre | editInfo = Maybe.map (\x -> (lemmaSelect model,
                                                                 Tuple.second x))
                                                     editInfo
                    }
               else infoPre
        editButton name = case info.editInfo of
                           Nothing -> span [] []
                           Just _ -> button [ onClick <| Edit name
                                            , Attr.style Css.editButton
                                            ]
                                     [ text "✎" ]
        deleteButton name = button [ onClick <| Delete name
                                   , Attr.style Css.editButton ]
                            [ text "X" ]
    in
        case state of
            Hidden -> div [] []
            Visible editing -> div [ Attr.style Css.textField ]
                               [ div [ Attr.style Css.textFieldInner ]
                                 [ text <| capitalize name ]
                               , case editing of
                                     True -> (info.editInfo |>
                                                  Maybe.map Tuple.first |>
                                                  Maybe.withDefault widgets.textbox)
                                             contents
                                     False -> span [ Attr.style Css.textFieldEditContainer ]
                                              [ info.formatter (contents.value |>
                                                                    Maybe.withDefault "")
                                              , span [ Attr.style [("flex-grow", "2")]] []
                                              , editButton name
                                              , deleteButton name
                                              ]
                               ]

formView : Model -> Html Msg
formView ({lemmata, form, fieldStates} as model) =
    let
        f name = div [id ("formField-" ++ name)] [field model name]
        fields = OD.keys fieldInfo |> List.map f
    in

        div [ id "metadataForm" ] <|
            fields ++
                -- TODO: disable save button if no changes from original
            if List.any ((==) (Visible True)) <| Dict.values fieldStates
            then [ div [ Attr.style Css.saveButtonContainer ]
                       [ button [ onClick Save ] [ text "Save" ] ] ]
            else []

init : List Lemma -> Metadata -> Tree -> Model
init lemmata metadata node =
    let
        fieldNames = OD.keys fieldInfo
    in
        { form = fieldNames |>
                 List.map (\name -> Dict.get (String.toUpper name) metadata |>
                           Maybe.map (Form.Init.setString name)) |>
                 MX.values |>
                 flip Form.initial validation
        , fieldStates = fieldNames |>
                        List.map (\name ->
                                      let
                                          predicate = OD.get name fieldInfo |> fromJust |> .predicate
                                      in
                                          (name, if predicate node then Visible False else Hidden)) |>
                        Dict.fromList
        , lemmaSelectState = Select.newState "lemma"
        , lemmata = lemmata
        }

performUpdate : Dict String String -> Maybe Model ->
                String -> Updater ->
                Return Msg.Msg Tree -> Return Msg.Msg Tree
performUpdate formOut metadataForm field updater ret =
    let
        dirty = metadataForm |> -- TODO: could pass something smaller to this fn
                Maybe.map .fieldStates |>
                Maybe.andThen (Dict.get field) |>
                Maybe.map ((==) (Visible True)) |>
                Maybe.withDefault False
        fieldValue = Dict.get field formOut
        do = updater (String.toUpper field) fieldValue
    in
        if dirty
        then Return.andThen do ret
        else ret

save : Metadata -> BigModel.Model -> Return Msg.Msg BigModel.Model
save metadata model =
    let
        root = model |> .get Model.root
        selected = model |> .get Model.selected
    in
        case Selection.first selected of
            Just selection ->
                let
                    -- The Tree.l business is a bit weird/bad.  The underlying
                    -- problem is that our paths aren't guaranteed (by the
                    -- type system) to always be valid, but due to how we
                    -- generate them in practice they are.  So Tree.get
                    -- returns a Result, when it "should" return just a tree.
                    -- Options for fixing this are:
                    -- 1. Make Tree.get actually return a tree (or crash if it
                    -- somehow gets an invalid path?!)
                    -- 2. Use a better type for paths (e.g. zippers) that
                    -- guarantees their validity (but we tried this before and
                    -- it was a pain in the neck in other ways)
                    selectedNode = Tree.get selection root |> R.withDefault (Tree.l "foo" "bar")
                    doUpdate = performUpdate metadata model.metadataForm
                    getUpdater (key, info) = info.editInfo |> Maybe.map Tuple.second |> Maybe.map (doUpdate key)
                    updaters = OD.toList fieldInfo |> List.map getUpdater |> Maybe.Extra.values
                in
                    List.foldl (\x y -> x y) (Return.singleton selectedNode) updaters |>
                    Debug.log "after updates" |>
                    Return.map (\newLeaf -> Tree.set selection newLeaf root |>
                                        R.withDefault root) |>
                    Return.map (\x -> .set Model.root x model)
            _ -> Return.singleton model

update : BigModel.Model -> Msg -> Return Msg.Msg BigModel.Model
update model msg =
    case msg of
        ReceivedDefinition s -> case s of
                                    Success definition -> Return.singleton
                                                          { model | metadataForm =
                                                                Maybe.map (\x -> { x |
                                                                                   form = Form.update
                                                                                          validation
                                                                                          (Form.Input
                                                                                               "definition"
                                                                                               Form.Text
                                                                                               (Form.Field.String definition))
                                                                                          x.form
                                                                                 })
                                                                model.metadataForm
                                                          }
                                    _ -> Return.singleton model
        Form submsg -> Return.singleton { model | metadataForm =
                                              Maybe.map (\x -> { x |
                                                                 form = Form.update validation submsg x.form
                                                               })
                                              model.metadataForm
                                        }
        Edit fieldName -> Return.return { model | metadataForm =
                                              Maybe.map (\x -> { x |
                                                                 fieldStates = Dict.update
                                                                               fieldName
                                                                               (always <| Just <| Visible <| True)
                                                                               x.fieldStates
                                                               })
                                              model.metadataForm
                                        }
                          (TreeEdit.Ports.editing True)
        Delete fieldName ->
            let
                root = model |> .get Model.root
                selected = model |> .get Model.selected
            in
                case Selection.first selected of
                    Just path ->
                        let
                            selectedNode = Tree.get path root
                        in
                            selectedNode |>
                            R.map (Lens.modify Tree.metadata (Dict.update (String.toUpper fieldName) (always Nothing))) |>
                            R.andThen (\newLeaf -> Tree.set path newLeaf root) |>
                            R.map (\x -> .set Model.root x model) |>
                            R.handle model |>
                            message (Msg.Metadata NewSelection)
                    Nothing -> Return.singleton model
        Save ->
            let
                _ = model.metadataForm |> Maybe.map (.form >> Form.getErrors) |> Debug.log "out"
            in
                case model.metadataForm |> Maybe.andThen (.form >> Form.getOutput) |> Debug.log "out" of
                    Just metadata -> save metadata model |>
                                     message (Msg.Metadata NewSelection) |>
                                     Return.command (TreeEdit.Ports.editing False)
                    Nothing -> Return.singleton model
        Cancel -> Return.singleton model |>
                  message (Msg.Metadata NewSelection) |>
                  Return.command (TreeEdit.Ports.editing False)
                  -- TODO: why doesn't it work?
                  -- (Cmd.batch [TreeEdit.Ports.editing False, Cmd.Extra.perform <| Msg.Metadata NewSelection])
        NewSelection ->
            let
                root = model |> .get Model.root
            in
                case Selection.getOne model.selected of
                    Just p ->
                        let
                            node = Tree.get p root |> R.withDefault (Tree.l "foo" "bar")
                            metadata = (.get Tree.metadata) node
                        in
                            case Tree.isTerminal node of
                                True ->
                                    let
                                        lemma = Dict.get "LEMMA" metadata
                                        req x = Net.get
                                                (Net.url "/dictentry" [("lemma", x)])
                                                ReceivedDefinition
                                                D.string
                                    in
                                        Return.return { model | metadataForm = Just <|
                                                            init (Model.lemmata model) metadata node
                                                      }
                                            (lemma |> Maybe.map req |>
                                                 Maybe.withDefault Cmd.none |> Cmd.map Msg.Metadata)
                                False -> Return.singleton
                                         { model | metadataForm =
                                               Just <| init (Model.lemmata model) metadata node
                                         }
                    _ -> Return.singleton { model | metadataForm = Nothing }
        SaveSuccess lemma -> Return.singleton { model | lastMessage = "Saved definition for lemma " ++ lemma }
        Key {keyCode} -> case keyCode of
                        K.Escape -> Return.return { model | metadataForm = Nothing } (TreeEdit.Ports.editing False)
                        _ -> Return.singleton model
        LemmaSelect msg ->
            let
                -- State can be nothing in the case where we have used ESC to
                -- cancel editing the metadata entirely.  So we need to handle
                -- that case; otherwise this logic would be simpler
                state_ = model.metadataForm |> Maybe.map .lemmaSelectState
            in
                case state_ of
                    Just state ->
                        let
                            (newState, cmd) = Select.update lemmaSelectConfig msg state
                            newForm = model.metadataForm |> Maybe.map (\x -> { x | lemmaSelectState = newState })
                        in
                            cmd |>
                            Cmd.map Msg.Metadata |>
                            Return.return { model | metadataForm = newForm }
                    Nothing -> Return.singleton model

view : BigModel.Model -> Html Msg
view model =
    case model.metadataForm of
        Just model -> div [ id "metadata-editor"
                                  , Attr.style [ ("background-color", theme.offWhite2)
                                               , ("padding-bottom", "2px")
                                               ]
                                  ]
                              [ div [ Attr.style [ ("background-color", theme.darkGrey)
                                                 , ("color", "white")
                                                 , ("width", "100%")
                                                 , ("height", "16px")
                                                 , ("font-weight", "bold")
                                                 , ("text-align", "center")
                                                 ]
                                    ] [ text "Metadata" ]
                              , formView model
                              ]
        Nothing -> div [] []
