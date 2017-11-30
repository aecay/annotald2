module TreeEdit.Metadata exposing (view, update)

import Css exposing ( backgroundColor, paddingBottom, px, pct, color
                    , width, height,fontWeight, bold, textAlign, center
                    , rgb)
import Css.Colors exposing (white)
import Cmd.Extra
import Dict exposing (Dict)
import Html.Styled as Html exposing (div, text, button, Html, span)
import Html.Styled.Attributes as Attr exposing (id)
import Html.Styled.Events exposing (onClick)
import Html.Attributes as UnstyledAttr -- TODO: ugly
import Http exposing (encodeUri)
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
import Form.Input as Input
import Form.Init
import Form.Field
import Form.Validate as V exposing (Validation)

import TreeEdit.Metadata.Type exposing (..)
import TreeEdit.Metadata.Css as Css
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

origTagState : Tree -> Bool
origTagState t =
    Tree.isTerminal t && (.get Tree.metadata t |> Dict.get "OLD-TAG" |> (/=) Nothing)

fieldNames : List (String, Tree -> Bool)
fieldNames = [ ("lemma", Tree.isTerminal)
             , ("definition", Tree.isTerminal)
             , ("old-tag", origTagState)
             , ("case", Tree.isTerminal) -- TODO: improvements
             , ("gender", Tree.isTerminal) -- TODO: improvements
             , ("number", Tree.isTerminal) -- TODO: improvements
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
        List.foldl do init (List.map Tuple.first fieldNames)

capitalize : String -> String
capitalize s = String.toUpper (String.left 1 s) ++ String.dropLeft 1 s

type FieldType = Writable | ReadOnly

textField : FieldStates -> MetadataForm -> FieldType -> String -> (String -> Html Msg) -> Html Msg
textField fs form fieldType name format =
    let
        state = Dict.get name fs |> Maybe.withDefault Hidden
        contents = Form.getFieldAsString name form
        editButton name = case fieldType of
                           ReadOnly -> span [] []
                           Writable -> button [ onClick <| Edit name , Css.editButton ] [ text "✎" ]
        deleteButton name = button [ onClick <| Delete name , Css.editButton ] [ text "X" ]
    in
        case state of
            Hidden -> div [] []
            _ -> div [ Css.textField ]
                 [ div [ Css.textFieldInner ]
                       [ text <| capitalize name ]
                 , case state of
                       Editing -> Html.map Form <| Html.fromUnstyled <|
                                  Input.textInput contents [ UnstyledAttr.style [("width", "100%")] ]
                       Visible -> span [ Css.textFieldEditContainer ]
                                  [ format (contents.value |> Maybe.withDefault "")
                                  , span [ Attr.style [("flex-grow", "2")]] []
                                  , editButton name
                                  , deleteButton name
                                  ]
                       Hidden -> Debug.crash "impossible"
                 ]

formatValue : String -> Html Msg
formatValue value =
    if value == ""
    then Html.i [ Css.textFieldAbsent ] [ text <| "not present" ]
    else text value

formatValueDefinition : String -> Html Msg
formatValueDefinition value =
    if (Debug.log "hi" value) == ""
    then Html.i [ Css.textFieldAbsent ] [ text <| "not present" ]
    else if String.startsWith "[de]" (Debug.log "hi" value)
         then
             let
                 linkify x = Html.a [Attr.href <| "https://www.dict.cc/?s=" ++ encodeUri x] [text x]
                 pieces = String.split "," (String.dropLeft 5 value) |>
                          List.map String.trim |>
                          List.map linkify
             in
                 Html.span [] <| List.intersperse (text ", ") pieces
         else text value

formView : MetadataForm -> FieldStates -> Html Msg
formView form state =
    let
        field name = div [id ("formField-" ++ name)] [textField state form Writable name formatValue]
        condField condition name = if condition then field name else div [] []
        roField name = textField state form ReadOnly name formatValue
        roCondField name = if (Form.getFieldAsString name form |> .value) /= Just ""
                           then roField name
                           else div [] []
    in

    div [ id "metadataForm" ] <|
        [ field "lemma"
        , if (Form.getFieldAsString "lemma" form |> .value) /= Just ""
          then div [id ("formField-" ++ "definition")]
              [textField state form Writable "definition" formatValueDefinition]
          else div [] []
        , roField "old-tag"
        , roCondField "case"
        , roCondField "gender"
        , roCondField "number"
        ] ++
        if List.any ((==) Editing) <| Dict.values state
        then [ div [ Css.saveButtonContainer ]
                   [ button [ onClick Save ] [ text "Save" ] ] ]
        else []

init : Metadata -> Tree -> (MetadataForm, FieldStates)
init metadata node = ( fieldNames |>
                           List.map (\(name, _) -> Dict.get (String.toUpper name) metadata |>
                                         Maybe.map (Form.Init.setString name)) |>
                           MX.values |>
                           flip Form.initial validation
                     , Dict.fromList <| List.map (\(name, initState) ->
                                                      (name, if initState node then Visible else Hidden))
                         fieldNames
                     )

type alias Updater = Maybe String -> Tree -> Return Msg.Msg Tree

genericU : String -> Updater
genericU key newVal sel =
    sel |>
    Lens.modify Tree.metadata (Dict.update (String.toUpper key) (always newVal)) |>
    Return.singleton

lemmaU : Updater
lemmaU = genericU "LEMMA"

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
                Maybe.map ((==) Editing) |>
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
                                              (Dict.update fieldName (always <| Just Editing))
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
                        in
                            case Tree.isTerminal node of
                                True ->
                                    let
                                        metadata = (.get Tree.metadata) node
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
                                False -> Return.singleton { model | metadataForm = Nothing }
                    _ -> Return.singleton { model | metadataForm = Nothing }
        SaveSuccess lemma -> Return.singleton { model | lastMessage = "Saved definition for lemma " ++ lemma }
        Key {keyCode} -> case keyCode of
                        K.Escape -> Return.return { model | metadataForm = Nothing } (TreeEdit.Ports.editing False)
                        _ -> Return.singleton model

view : Model -> Html Msg
view model =
    case model.metadataForm of
        Just (form, state) -> div [ id "metadata-editor"
                                  , Attr.css [ backgroundColor theme.offWhite2
                                             , paddingBottom (px 2)
                                             ]
                                  ]
                              [ div [ Attr.css [ backgroundColor theme.darkGrey
                                               , color white
                                               , width (pct 100)
                                               , height (px 16)
                                               , fontWeight bold
                                               , textAlign center
                                               ]
                                    ] [ text "Metadata" ]
                              , formView form state
                              ]
        Nothing -> div [] []
