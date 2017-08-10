module TreeEdit.Metadata exposing (view, update)

import RemoteData exposing (RemoteData(..), WebData)
import Return exposing (Return)
import Dict exposing (Dict)

import Html exposing (div, text, button, Html, span)
import Html.Attributes as Attr exposing (id)
import Html.Events exposing (onClick)

import Json.Decode as D
import Json.Encode as E

import Cmd.Extra

import RemoteData.Http as Net

import Form exposing (Form)
import Form.Input as Input
import Form.Init
import Form.Field
import Form.Validate as V exposing (Validation)

import Monocle.Optional as Optional
import Monocle.Common exposing (first, second, maybe)

import TreeEdit.Metadata.Type exposing (..)
import TreeEdit.Selection exposing (Selection)
import TreeEdit.Tree as Tree
import TreeEdit.Model as Model
import TreeEdit.Selection as Selection
import TreeEdit.Result as R

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
        div [ Attr.style [ ("border", "1px solid #2E2E2E")
                         , ("margin", "2px")
                         ]
            ]
            [ div [ Attr.style [ ("background-color", "#555555")
                               , ("color", "#eeeeee")
                               , ("font-weight", "bold")
                               , ("text-align", "center")
                               , ("width", "100%")
                               , ("height", "16px")
                               ]
                  ]
                  [ text <| capitalize name ]
            , if editing
              then Html.map Form <| Input.textInput contents [ Attr.style [ ("width", "100%") ] ]
              else span [ Attr.style [ ("display", "flex")
                                     , ("justify-content", "space-between")
                                     , ("align-items", "center")
                                     , ("padding", "2px")
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
        then [ div [Attr.style [ ("margin", "2px")
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

save : Metadata -> Model.Model -> Return Msg Model.Model
save metadata model =
    case (model.root, Selection.first model.selected) of
        (Success root, Just selection) ->
            let
                {lemma, definition} = metadata
                sel = Tree.get selection root
                -- set : R.Result (Dict String String -> Tree.Tree)
                set = R.map (flip (.set Tree.metadata)) sel
                newSel = sel |>
                         R.map (.get Tree.metadata) |>
                         R.map (Dict.update "lemma" (always <| Just lemma)) |>
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
                    { model | root = Success newRoot }
                    updateCmd
        _ -> Return.singleton model

update : Model.Model -> Msg -> Return Msg Model.Model
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
        Edit fieldName -> Return.singleton { model | metadataForm =
                                                 Optional.modify
                                                 (Optional.composeLens maybe second)
                                                 (Dict.update fieldName (always <| Just True))
                                                 model.metadataForm
                                           }
        Save -> case model.metadataForm |> Maybe.andThen (Tuple.first >> Form.getOutput) of
                    Just metadata -> save metadata model |> Return.command (Cmd.Extra.perform NewSelection)
                    Nothing -> Return.singleton model
        Cancel -> Return.singleton { model | metadataForm = Nothing }
        NewSelection ->
            case (model.root, Selection.first model.selected) of
                -- TODO: first returns Just for a two-element selection
                (Success root, Just p) ->
                    case (Tree.get p root) |> R.map Tree.isTerminal |> R.withDefault False of
                        True ->
                            let
                                dict = (Tree.get p root) |> R.map (.get Tree.metadata) |> R.withDefault Dict.empty
                                extract key = Dict.get key dict
                                lemma = extract "lemma"
                                req x = Net.get
                                        (Net.url "/dictentry" [("lemma", x)])
                                        ReceivedDefinition
                                        D.string
                            in
                                Return.return { model | metadataForm = Just <|
                                                    init { lemma = Maybe.withDefault "" lemma
                                                         , definition = Nothing }
                                              }
                                (lemma |> Maybe.map req |> Maybe.withDefault Cmd.none)
                        False -> Return.singleton { model | metadataForm = Nothing }
                _ -> Return.singleton { model | metadataForm = Nothing }
        SaveSuccess lemma -> Return.singleton { model | lastMessage = "Saved definition for lemma " ++ lemma }

view : Model.Model -> Html Msg
view model =
    case model.metadataForm of
        Just (form, state) -> div [ id "metadata-editor"
                                  , Attr.style [ ("background", "#FEF6EA")
                                               , ("padding-bottom", "2px")
                                               ]
                                  ]
                              [ div [ Attr.style [ ("background-color", "#2E2E2E")
                                                 , ("color", "white")
                                                 , ("font-weight", "bold")
                                                 , ("text-align", "center")
                                                 , ("width", "100%")
                                                 , ("height", "16px")
                                                 ]
                                    ] [ text "Metadata" ]
                              , formView form state
                              ]
        Nothing -> div [] []
