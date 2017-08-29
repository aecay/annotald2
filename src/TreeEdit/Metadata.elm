module TreeEdit.Metadata exposing (view, update)

import Cmd.Extra
import Color exposing (rgb, white)
import Dict exposing (Dict)
import Html exposing (div, text, button, Html, span)
import Html.Attributes as Attr exposing (id)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
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
import TreeEdit.Selection exposing (Selection)
import TreeEdit.Tree as Tree
import TreeEdit.Model as Model
import TreeEdit.Model.Type exposing (Model)
import TreeEdit.Ports
import TreeEdit.Selection as Selection
import TreeEdit.Result as R
import TreeEdit.View.Theme exposing (theme)
import TreeEdit.Msg as Msg

validation : Validation () Metadata
validation =
    V.map2 (\l def -> { lemma = l, definition = def })
        (V.field "lemma" (V.string |> V.andThen V.nonEmpty))
        (V.field "definition" (V.string |> V.map Just)) -- TODO: should be Nothing
                                                  -- if empty

capitalize : String -> String
capitalize s = String.toUpper (String.left 1 s) ++ String.dropLeft 1 s

textField : FieldState -> MetadataForm -> String -> Html Msg
textField fs form name =
    let
        editing = Dict.get name fs |> Maybe.withDefault False
        contents = Form.getFieldAsString name form
        formatValue x =
            let
                value = x.value |> Maybe.withDefault ""
            in
                if value == ""
                then Html.i [ Attr.style [ ("color", "grey") ] ] [ text <| "no " ++ name ]
                else text value
        editButton m = button [ onClick m , Attr.style [("padding", "1px")]] [ text "✎" ]
    in
        div [ Attr.style [ border 1 px solid theme.darkGrey
                         , margin 2 px
                         ]
            ]
            [ div [ Attr.style [ backgroundColor (rgb 85 85 85)
                               , color (rgb 238 238 238)
                               , width 100 prc
                               , height 16 px
                               , ("font-weight", "bold")
                               , textCenter
                               ]
                  ]
                  [ text <| capitalize name ]
            , if editing
              then Html.map Form <| Input.textInput contents [ Attr.style [ ("width", "100%") ] ]
              else span [ Attr.style [ ("display", "flex")
                                     , ("justify-content", "space-between")
                                     , ("align-items", "center")
                                     , padding 2 px
                                     ]
                        ]
                  [ formatValue contents
                  , span [ Attr.style [("flex-grow", "2")]] []
                  , editButton <| Edit name
                  ]
            ]

formView : MetadataForm -> FieldState -> Html Msg
formView form state =
    let
        field = textField state form
    in

    div [ id "lemma" ] <|
        [ div [ id "lemma-text" ]
              [ field "lemma" ]
        ] ++
        (if (Form.getFieldAsString "lemma" form |> .value) == Just ""
         then []
         else [ div [ id "lemma-def" ]
                    [ field "definition" ]
              ]
        ) ++
        if List.any identity <| Dict.values state
        then [ div [Attr.style [ margin 2 px
                               , ("display", "flex")
                               , ("flex-direction", "row-reverse")
                               ]
                   ]
                   [ button [ onClick Save ] [ text "Save" ] ] ]
        else []

init : Metadata -> (MetadataForm, FieldState)
init {lemma, definition} = ( Form.initial [ Form.Init.setString "lemma" lemma
                                          , Form.Init.setString "definition" <| Maybe.withDefault "" definition
                                          ]
                                 validation
                           , Dict.fromList [ ("lemma", False)
                                           , ("definition", False)
                                           ]
                           )

save : Metadata -> Model -> Return Msg.Msg Model
save metadata model =
    let
        root = model |> .get Model.root
        selected = model |> .get Model.selected
    in
        case Selection.first selected of
            Just selection ->
                let
                    {lemma, definition} = metadata
                    sel = Tree.get selection root
                    set = R.map (flip (.set Tree.metadata)) sel
                    newSel = sel |>
                             R.map (.get Tree.metadata) |>
                             R.map (Dict.update "LEMMA" (always <| Just lemma)) |>
                             flip R.andMap set
                    newRoot = newSel |> R.andThen (\x -> Tree.set selection x root) |> R.withDefault root
                    updateCmd = case (definition,
                                          model.metadataForm |>
                                          Maybe.map Tuple.second |>
                                          Maybe.andThen (Dict.get "definition"))
                                of
                                    (Just def, Just True) ->
                                        Net.post
                                            "/dictentry"
                                            (always <| SaveSuccess lemma)
                                            (D.succeed ())
                                            (E.object [ ("lemma", E.string lemma)
                                                      , ("definition", E.string def)
                                                      ])
                                    _ -> Cmd.none

                in
                    Return.return
                        (.set Model.root newRoot model)
                        (Cmd.map Msg.Metadata updateCmd)
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
        Cancel -> Return.return { model | metadataForm = Nothing } (TreeEdit.Ports.editing False)
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
                                                        init { lemma = Maybe.withDefault "" lemma
                                                             , definition = Nothing }
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
