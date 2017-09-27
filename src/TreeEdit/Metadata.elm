module TreeEdit.Metadata exposing (view, update)

import Cmd.Extra
import Color exposing (rgb, white)
import Dict exposing (Dict)
import Html exposing (div, text, button, Html, span)
import Html.Attributes as Attr exposing (id)
import Html.CssHelpers
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as MX
import Monocle.Lens as Lens
import Monocle.Optional as Optional
import Monocle.Common exposing (first, second, maybe)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Net
import Return exposing (Return)
import TypedStyles exposing ( px, border, solid, margin, width, height, prc
                            , color, backgroundColor, paddingBottom, padding
                            , textCenter)

import Form exposing (Form)
import Form.Input as Input
import Form.Init
import Form.Field
import Form.Validate as V exposing (Validation)

import TreeEdit.Metadata.Type exposing (..)
import TreeEdit.Metadata.Css exposing (Classes(..))
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
import TreeEdit.View.Css exposing (ns)

{id, class, classList} = Html.CssHelpers.withNamespace ns

fieldNames = [ "lemma"
             , "definition"
             , "orig-tag"
             ]

validation : Validation () Metadata
validation =
    let
        do str = V.andThen (V.string |> V.map (Dict.update str (always str)))
    in
        List.foldl do (V.succeed Dict.empty) fieldNames

capitalize : String -> String
capitalize s = String.toUpper (String.left 1 s) ++ String.dropLeft 1 s

type FieldType = Writable | ReadOnly

textField : FieldState -> MetadataForm -> FieldType -> String -> Html Msg
textField fs form fieldType name =
    let
        editing = Dict.get name fs |> Maybe.withDefault False
        contents = Form.getFieldAsString name form
        formatValue x =
            let
                value = x.value |> Maybe.withDefault ""
            in
                if value == ""
                then Html.i [ class [TextFieldAbsent] ] [ text <| "no " ++ name ]
                else text value
        editButton m = case fieldType of
                           ReadOnly -> span [] []
                           Writable -> button [ onClick m , class [EditButton] ] [ text "✎" ]
    in
        div [ class [TextField] ]
            [ div [ class [TextFieldInner] ]
                  [ text <| capitalize name ]
            , if editing
              then Html.map Form <| Input.textInput contents [ class [TextFieldEditBox] ]
              else span [ class [TextFieldEditContainer] ]
                  [ formatValue contents
                  , span [ Attr.style [("flex-grow", "2")]] []
                  , editButton <| Edit name
                  ]
            ]

formView : MetadataForm -> FieldState -> Html Msg
formView form state =
    let
        field name = div [id ("formField-" ++ name)] [textField state form Writable name]
        condField condition name = if condition then field name else div [] []
        roField name = textField state form ReadOnly name
    in

    div [ id "metadataForm" ] <|
        [ field "lemma"
        , condField ((Form.getFieldAsString "lemma" form |> .value) == Just "") "definition"
        , roField "orig-tag"
        ] ++
        if List.any identity <| Dict.values state
        then [ div [ class [SaveButtonContainer] ]
                   [ button [ onClick Save ] [ text "Save" ] ] ]
        else []

init : Metadata -> (MetadataForm, FieldState)
init metadata = ( fieldNames |>
                      List.map (\x -> Dict.get x metadata) |>
                      MX.values |>
                      flip (Form.initial validation)
                , Dict.fromList <| List.map (\x -> (x, False)) fieldNames
                )

type alias Updater = String -> Tree -> Return Msg.Msg Tree

lemmaU : Updater
lemmaU newLemma sel =
    sel |>
    Lens.modify Tree.metadata Dict.update "LEMMA" (always <| Just newLemma) |>
    Return.singleton

definitionU : Updater
definitionU newDef sel =
    let
        lemma = (.get Tree.metadata) sel |> Dict.get "METADATA"
        updateCmd lemma = Net.post
                              "/dictentry"
                              (always <| SaveSuccess lemma)
                              (D.succeed ())
                              (E.object [ ("lemma", E.string lemma)
                                        , ("definition", E.string newDef)
                                        ])
    in
        Maybe.map updateCmd lemma |>
        Maybe.withDefault Cmd.none |>
        Return.return sel

performUpdate : Dict String String -> MetadataForm -> String -> Updater -> Return Msg.Msg Tree -> Return Msg.Msg Tree
performUpdate formOut metadataForm field updater ret =
    let
        dirty = metadataForm |> -- TODO: could pass something smaller to this fn
                Maybe.map Tuple.second |>
                Maybe.andThen (Dict.get field) |>
                Maybe.withDefault False
        fieldValue = Dict.get field formOut |> Maybe.withDefault ""
        do selected = updater fieldValue
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
                    doUpdate = R.andThen (performUpdate metadata model.metadataForm)

                in
                    Return.singleton (root |> Tree.get selection) |>
                    doUpdate "lemma" lemmaU |>
                    doUpdate "definition" definitionU |>
                    -- TODO doUpdate "orig-tag" origTagU |>
                    Return.andThen (Tree.set selection root) |>
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
                                              (Dict.update fieldName (always <| Just True))
                                              model.metadataForm
                                        }
                          (TreeEdit.Ports.editing True)
        Save -> case model.metadataForm |> Maybe.andThen (Tuple.first >> Form.getOutput) of
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
                        case (Tree.get p root) |> R.map Tree.isTerminal |> R.withDefault False of
                            True ->
                                let
                                    dict = (Tree.get p root) |> R.map (.get Tree.metadata) |> R.withDefault Dict.empty
                                    extract key = Dict.get key dict
                                    lemma = extract "LEMMA"
                                    req x = Net.get
                                            (Net.url "/dictentry" [("lemma", x)])
                                            ReceivedDefinition
                                            D.string
                                in
                                    Return.return { model | metadataForm = Just <|
                                                        init (Dict.fromList [("lemma", "")])
                                                  }
                                    (lemma |> Maybe.map req |> Maybe.withDefault Cmd.none |> Cmd.map Msg.Metadata)
                            False -> Return.singleton { model | metadataForm = Nothing }
                    _ -> Return.singleton { model | metadataForm = Nothing }
        SaveSuccess lemma -> Return.singleton { model | lastMessage = "Saved definition for lemma " ++ lemma }
        Key code -> case code of
                        27 -> Return.return { model | metadataForm = Nothing } (TreeEdit.Ports.editing False)
                        _ -> Return.singleton model

view : Model -> Html Msg
view model =
    case model.metadataForm of
        Just (form, state) -> div [ id "metadata-editor"
                                  , Attr.style [ backgroundColor theme.offWhite2
                                               , paddingBottom 2 px
                                               ]
                                  ]
                              [ div [ Attr.style [ backgroundColor theme.darkGrey
                                                 , color white
                                                 , width 100 prc
                                                 , height 16 px
                                                 , ("font-weight", "bold")
                                                 , textCenter
                                                 ]
                                    ] [ text "Metadata" ]
                              , formView form state
                              ]
        Nothing -> div [] []
