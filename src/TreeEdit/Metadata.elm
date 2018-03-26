module TreeEdit.Metadata exposing (view, update)

import Cmd.Extra
import Dict exposing (Dict)
import Html as Html exposing (div, text, button, Html, span, ul, li)
import Html.Attributes as Attr exposing (id)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Keyboard.Key as K
import Maybe.Extra as MX
import Monocle.Lens as Lens
import Monocle.Optional as Optional
import Monocle.Common exposing (first, second, maybe)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Net
import Return exposing (Return)

import Form exposing (Form)
import Form.Init
import Form.Field
import Form.Validate as V exposing (Validation)

import TreeEdit.Metadata.Type exposing (..)
import TreeEdit.Metadata.Css as Css
import TreeEdit.Metadata.Util exposing (..)
import TreeEdit.Selection exposing (Selection)
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Model as Model
import TreeEdit.Model.Type exposing (Model)
import TreeEdit.Ports
import TreeEdit.Selection as Selection
import TreeEdit.Result as R
import TreeEdit.View.Theme exposing (theme)
import TreeEdit.Msg as Msg
import TreeEdit.Utils exposing (fromJust)

type alias FieldInfo = { predicate : Tree -> Bool
                       , formatter : Formatter
                       , widget : Maybe EditWidget
                       }

lemma      : FieldInfo
lemma      = FieldInfo Tree.isTerminal                 formatters.value      <| Just widgets.textbox
definition : FieldInfo
definition = FieldInfo Tree.isTerminal                 formatters.definition <| Just widgets.textbox
gender     : FieldInfo
gender     = FieldInfo isNominal                       formatters.value      Nothing
number     : FieldInfo
number     = FieldInfo (\x -> isNominal x || isVerb x) formatters.value      Nothing

case_ : FieldInfo
case_ =
    let
        caseOptions = ["nom", "gen", "dat", "akk"]
    in
        FieldInfo isNominal formatters.value <| Just <| widgets.options caseOptions

fieldInfo : Dict String FieldInfo
fieldInfo = Dict.fromList
            [ ("lemma", lemma)
            , ("definition", definition)
            , ("old-tag", FieldInfo (hasMetadata "OLD-TAG") formatters.value Nothing)
            , ("case", case_)
            , ("gender", gender)
            , ("number", number)
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
        List.foldl do init (Dict.keys fieldInfo)

field : FieldStates -> MetadataForm -> String -> Html Msg
field fs form name =
    let
        state = Dict.get name fs |> Maybe.withDefault Hidden
        contents = Form.getFieldAsString name form
        info = Dict.get name fieldInfo |> fromJust
        editButton name = case info.widget of
                           Nothing -> span [] []
                           Just _ -> button [ onClick <| Edit name , Attr.style Css.editButton ] [ text "✎" ]
        deleteButton name = button [ onClick <| Delete name , Attr.style Css.editButton ] [ text "X" ]
    in
        case state of
            Hidden -> div [] []
            Visible editing -> div [ Attr.style Css.textField ]
                               [ div [ Attr.style Css.textFieldInner ]
                                 [ text <| capitalize name ]
                               , case editing of
                                     True -> (info.widget |> Maybe.withDefault widgets.textbox) contents
                                     False -> span [ Attr.style Css.textFieldEditContainer ]
                                              [ info.formatter (contents.value |> Maybe.withDefault "")
                                              , span [ Attr.style [("flex-grow", "2")]] []
                                              , editButton name
                                              , deleteButton name
                                              ]
                               ]

formView : MetadataForm -> FieldStates -> Html Msg
formView form state =
    let
        formatter name = fieldInfo |>
                         Dict.get name |>
                         fromJust |>
                         .formatter
        f name = div [id ("formField-" ++ name)] [field state form name]
        -- condField condition name = if condition then field name else div [] []
    in

        div [ id "metadataForm" ] <|
            [ f "lemma"
            , if (Form.getFieldAsString "lemma" form |> .value) /= Just "" -- TODO: smelly
              then f "definition"
              else div [] []
            , f "old-tag"
            , f "validation-error"
            , f "case"
            , f "gender"
            , f "number"
            ] ++
            if List.any ((==) (Visible True)) <| Dict.values state
            then [ div [ Attr.style Css.saveButtonContainer ]
                       [ button [ onClick Save ] [ text "Save" ] ] ]
            else []

init : Metadata -> Tree -> (MetadataForm, FieldStates)
init metadata node =
    let
        fieldNames = Dict.keys fieldInfo
    in

        ( fieldNames |>
              List.map (\name -> Dict.get (String.toUpper name) metadata |>
                            Maybe.map (Form.Init.setString name)) |>
              MX.values |>
              flip Form.initial validation
        , fieldNames |>
              List.map (\name ->
                            let
                                predicate = Dict.get name fieldInfo |> fromJust |> .predicate
                            in
                                (name, if predicate node then Visible False else Hidden)) |>
              Dict.fromList
        )

type alias Updater = Maybe String -> Tree -> Return Msg.Msg Tree

genericU : String -> Updater
genericU key newVal sel =
    sel |>
    Lens.modify Tree.metadata (Dict.update (String.toUpper key) (always newVal)) |>
    Return.singleton

lemmaU : Updater
lemmaU = genericU "LEMMA"

caseU : Updater
caseU = genericU "CASE"

oldTagU : Updater
oldTagU = genericU "OLD-TAG"

definitionU : Updater
definitionU newDef sel =
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

performUpdate : Dict String String -> Maybe (MetadataForm, FieldStates) ->
                String -> Updater ->
                Return Msg.Msg Tree -> Return Msg.Msg Tree
performUpdate formOut metadataForm field updater ret =
    let
        dirty = metadataForm |> -- TODO: could pass something smaller to this fn
                Maybe.map Tuple.second |>
                Maybe.andThen (Dict.get field) |>
                Maybe.map ((==) (Visible True)) |>
                Maybe.withDefault False
        fieldValue = Dict.get field formOut
        do = updater fieldValue
    in
        if dirty
        then Return.andThen do ret
        else ret

save : Metadata -> Model -> Return Msg.Msg Model
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
                in
                    Return.singleton selectedNode |> -- TODO: automatically
                                                     -- enumerate metadata fields
                    doUpdate "lemma" lemmaU |>
                    doUpdate "definition" definitionU |>
                    doUpdate "old-tag" oldTagU |>
                    doUpdate "case" caseU |>
                    Debug.log "after updates" |>
                    Return.map (\newLeaf -> Tree.set selection newLeaf root |>
                                        R.withDefault root) |>
                    Return.map (\x -> .set Model.root x model)
            _ -> Return.singleton model

update : Model -> Msg -> Return Msg.Msg Model
update model msg =
    case msg of
        ReceivedDefinition s -> case s of
                                    Success definition -> Return.singleton
                                                          { model | metadataForm =
                                                                Optional.modify
                                                                (Optional.composeLens maybe first)
                                                                (Form.update
                                                                     validation
                                                                     (Form.Input "definition" Form.Text (Form.Field.String definition)))
                                                                model.metadataForm
                                                          }
                                    _ -> Return.singleton model
        Form submsg -> Return.singleton { model | metadataForm =
                                              Optional.modify
                                              (Optional.composeLens maybe first)
                                              (Form.update validation submsg)
                                              model.metadataForm
                                        }
        Edit fieldName -> Return.return { model | metadataForm =
                                              Optional.modify
                                              (Optional.composeLens maybe second)
                                              (Dict.update fieldName (always <| Just <| Visible <| True))
                                              model.metadataForm
                                        }
                          (TreeEdit.Ports.editing True)
        Delete fieldName -> let
            root = model |> .get Model.root
            selected = model |> .get Model.selected
        in
            case Selection.first selected of
                Just path ->
                    let
                        selectedNode = Tree.get path root |> R.withDefault (Tree.l "foo" "bar")
                    in
                        Return.singleton selectedNode |>
                        Return.andThen (genericU fieldName Nothing) |>
                        Return.map (\newLeaf -> Tree.set path newLeaf root |>
                                        R.withDefault root) |>
                        Return.map (\x -> .set Model.root x model) |>
                        Return.command (Cmd.Extra.perform (Msg.Metadata NewSelection))
                Nothing -> Return.singleton model
        Save ->
            let
                _ = model.metadataForm |> Maybe.map (Tuple.first >> Form.getErrors) |> Debug.log "out"
            in
                case model.metadataForm |> Maybe.andThen (Tuple.first >> Form.getOutput) |> Debug.log "out" of
                    Just metadata -> save metadata model |>
                                     Return.command (Cmd.map Msg.Metadata <| Cmd.Extra.perform NewSelection) |>
                                     Return.command (TreeEdit.Ports.editing False)
                    Nothing -> Return.singleton model
        Cancel -> Return.singleton model |>
                  Return.command (Cmd.map Msg.Metadata <| Cmd.Extra.perform NewSelection) |>
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
                                                            init metadata node
                                                      }
                                            (lemma |> Maybe.map req |>
                                                 Maybe.withDefault Cmd.none |> Cmd.map Msg.Metadata)
                                False -> Return.singleton { model | metadataForm =  Just <| init metadata node}
                    _ -> Return.singleton { model | metadataForm = Nothing }
        SaveSuccess lemma -> Return.singleton { model | lastMessage = "Saved definition for lemma " ++ lemma }
        Key {keyCode} -> case keyCode of
                        K.Escape -> Return.return { model | metadataForm = Nothing } (TreeEdit.Ports.editing False)
                        _ -> Return.singleton model

view : Model -> Html Msg
view model =
    case model.metadataForm of
        Just (form, state) -> div [ id "metadata-editor"
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
                              , formView form state
                              ]
        Nothing -> div [] []
