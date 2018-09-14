module TreeEdit.Metadata exposing (update, view)

import Array
import Dict exposing (Dict)
import Html as Html exposing (Html, button, div, li, span, text, ul)
import Html.Attributes as Attr exposing (id)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Url.Builder exposing (absolute, string)


import Form exposing (Form)
import Form.Field
import Form.Init
import Form.Validate as V exposing (Validation)
import Maybe.Extra as MX
import Monocle.Lens as Lens
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Net
import Return exposing (Return)
import Select

import ThirdParty.KeyboardKey as K

import TreeEdit.Metadata.Css as Css
import TreeEdit.Metadata.Type exposing (..)
import TreeEdit.Metadata.Util exposing (..)
import TreeEdit.Model as Model
import TreeEdit.Model.Type as BigModel
import TreeEdit.Msg as Msg
import TreeEdit.OrderedDict as OD
import TreeEdit.Ports
import TreeEdit.Result as R
import TreeEdit.Selection as Selection exposing (Selection)
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Utils exposing (message)
import TreeEdit.View.Theme exposing (theme)


type alias Updater =
    String -> Maybe String -> Tree -> Return Msg.Msg Tree


fnUpdater : (String -> String) -> Updater
fnUpdater fn key newVal sel =
    sel
        |> Lens.modify Tree.metadata (Dict.update key (always <| Maybe.map fn newVal))
        |> Return.singleton


genericUpdater : Updater
genericUpdater =
    fnUpdater identity


definitionUpdater : Updater
definitionUpdater _ newDef sel =
    let
        l =
            .get Tree.metadata sel |> Dict.get "LEMMA"

        updateCmd def lem =
            Net.post
                "/dictentry"
                (always <| Msg.Loaded <| Msg.Metadata <| SaveSuccess lem)
                (D.succeed ())
                (E.object
                    [ ( "lemma", E.string lem )
                    , ( "definition", E.string def )
                    ]
                )
    in
    Maybe.map2 updateCmd newDef l
        |> Maybe.withDefault Cmd.none
        |> Return.return sel


type alias FieldInfo =
    { predicate : Tree -> Bool
    , formatter : Formatter
    , editInfo : Maybe ( EditWidget, Updater )
    }


lemma : FieldInfo
lemma =
    FieldInfo Tree.isTerminal formatters.value <|
        Just
            ( widgets.textbox
            , -- The widget will actually
              -- get swapped out below as
              -- a special case,
              -- since it needs to take the
              -- model as an argument
              genericUpdater
            )


definition : FieldInfo
definition =
    FieldInfo (hasMetadata "LEMMA") formatters.definition <| Just ( widgets.textbox, definitionUpdater )


origTag : FieldInfo
origTag =
    FieldInfo Tree.isTerminal formatters.value <| Just ( widgets.textbox, fnUpdater String.toUpper )


mood : FieldInfo
mood =
    let
        moods =
            [ "ind", "konj" ]
    in
    FieldInfo isVerb formatters.value <| Just ( widgets.options moods, genericUpdater )


case_ : FieldInfo
case_ =
    let
        caseOptions =
            [ "nom", "gen", "dat", "akk" ]

        pred =
            eitherP isAdjective <| eitherP isNominal isPreposition
    in
    FieldInfo pred formatters.value <| Just ( widgets.options caseOptions, genericUpdater )


number : FieldInfo
number =
    let
        numOptions =
            [ "sg", "pl" ]

        pred =
            eitherP isNominal isVerb
    in
    FieldInfo pred formatters.value <| Just ( widgets.options numOptions, genericUpdater )


gender : FieldInfo
gender =
    let
        genderOptions =
            [ "masc", "fem", "neut" ]
    in
    FieldInfo isNominal formatters.value <| Just ( widgets.options genderOptions, genericUpdater )


fieldInfo : OD.OrderedDict String FieldInfo
fieldInfo =
    OD.fromList
        [ ( "lemma", lemma )
        , ( "definition", definition )
        , ( "orig-tag", origTag )
        , ( "case", case_ )
        , ( "gender", gender )
        , ( "number", number )
        , ( "mood", mood )
        , ( "validation-error", FieldInfo (hasMetadata "VALIDATION-ERROR") formatters.validationError Nothing )
        ]


validation : Validation () Metadata
validation =
    let
        upd str dict val =
            Dict.update str (always <| val) dict

        do : String -> Validation () Metadata -> Validation () Metadata
        do str val =
            val
                |> V.andThen (\dict -> V.field str (V.string |> V.maybe |> V.map (upd str dict)))

        ini =
            V.succeed Dict.empty
    in
    List.foldl do ini <| Array.toList <| OD.keys fieldInfo


field : Model -> String -> FieldInfo -> Html Msg
field model name info_ =
    let
        state = Dict.get name model.fieldStates |> Maybe.withDefault Hidden
        contents = Form.getFieldAsString name model.form

        info = info_
                 |> (\x -> if name == "lemma"
                           then { x | editInfo = Maybe.map (Tuple.mapFirst (always <| lemmaSelect model)) x.editInfo}
                           else x
                    )

        editButton n =
            case info.editInfo of
                Nothing ->
                    span [] []

                Just _ ->
                    button
                        ((onClick <| Edit n) :: Css.editButton)
                        [ text "✎" ]

        deleteButton n =
            button
                ((onClick <| Delete n) :: Css.editButton)
                [ text "X" ]
    in
    case state of
        Hidden ->
            div [] []

        Visible editing ->
            div Css.textField
                [ div Css.textFieldInner
                      [ text <| capitalize name ]
                , case editing of
                    True ->
                        (info.editInfo
                            |> Maybe.map Tuple.first
                            |> Maybe.withDefault widgets.textbox
                        )
                            contents

                    False ->
                        span Css.textFieldEditContainer
                            [ info.formatter
                                (contents.value
                                    |> Maybe.withDefault ""
                                )
                            , span [ Attr.style "flex-grow" "2" ] []
                            , editButton name
                            , deleteButton name
                            ]
                ]


formView : Model -> Html Msg
formView ({ lemmata, form, fieldStates } as model) =
    let
        f (name, info) =
            div [ id ("formField-" ++ name) ] [ field model name info ]

        fields = fieldInfo |> OD.toList |> List.map f
    in
    div [ id "metadataForm" ] <|
        fields
            ++ -- TODO: disable save button if no changes from original
               (if List.any ((==) (Visible True)) <| Dict.values fieldStates then
                    [ div Css.saveButtonContainer
                          [ button [ onClick Save ] [ text "Save" ] ]
                    ]

                else
                    []
               )


init : List Lemma -> Metadata -> Tree -> Model
init lemmata metadata node =
    let
        fieldNames = OD.keys fieldInfo |> Array.toList
    in
    { form =
        fieldNames
            |> List.map
                (\name ->
                    Dict.get (String.toUpper name) metadata
                        |> Maybe.map (Form.Init.setString name)
                )
            |> MX.values
            |> (\a -> Form.initial a validation)
    , fieldStates =
        fieldInfo
          |> OD.toList
          |> List.map
                (\(name, info) ->
                    ( name
                    , if info.predicate node
                      then Visible False
                      else Hidden
                    )
                )
            |> Dict.fromList
    , lemmaSelectState = Select.newState "lemma"
    , lemmata = lemmata
    }


performUpdate :
    Dict String String
    -> Maybe Model
    -> String
    -> Updater
    -> Return Msg.Msg Tree
    -> Return Msg.Msg Tree
performUpdate formOut metadataForm fld updater ret =
    let
        dirty =
            metadataForm
                |> -- TODO: could pass something smaller to this fn
                   Maybe.map .fieldStates
                |> Maybe.andThen (Dict.get fld)
                |> Maybe.map ((==) (Visible True))
                |> Maybe.withDefault False

        fieldValue =
            Dict.get fld formOut

        do =
            updater (String.toUpper fld) fieldValue
    in
    if dirty then
        Return.andThen do ret

    else
        ret


save : Metadata -> BigModel.ForestModel -> Return Msg.Msg BigModel.ForestModel
save metadata model =
    let
        root = model.root
        selected = model.selected
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
                selectedNode =
                    Tree.get selection root

                doUpdate =
                    performUpdate metadata model.metadataForm

                getUpdater ( key, info ) =
                    info.editInfo |> Maybe.map Tuple.second |> Maybe.map (doUpdate key)

                updaters =
                    OD.toList fieldInfo |> List.map getUpdater |> MX.values
            in
            List.foldl (\x y -> x y) (Return.singleton selectedNode) updaters
                |> Return.map (\newLeaf -> Tree.set selection newLeaf root)
                |> Return.map (\x -> { model | root = x })

        _ ->
            Return.singleton model


update : BigModel.ForestModel -> Msg -> Return Msg.Msg BigModel.ForestModel
update model msg =
    case msg of
        ReceivedDefinition s ->
            case s of
                Success defn ->
                    Return.singleton
                        { model
                            | metadataForm =
                                Maybe.map
                                    (\x ->
                                        { x
                                            | form =
                                                Form.update
                                                    validation
                                                    (Form.Input
                                                        "definition"
                                                        Form.Text
                                                        (Form.Field.String defn)
                                                    )
                                                    x.form
                                        }
                                    )
                                    model.metadataForm
                        }

                _ ->
                    Return.singleton model

        Form submsg ->
            Return.singleton
                { model
                    | metadataForm =
                        Maybe.map
                            (\x ->
                                { x
                                    | form = Form.update validation submsg x.form
                                }
                            )
                            model.metadataForm
                }

        Edit fieldName ->
            Return.return
                { model
                    | metadataForm =
                        Maybe.map
                            (\x ->
                                { x
                                    | fieldStates =
                                        Dict.update
                                            fieldName
                                            (always <| Just <| Visible <| True)
                                            x.fieldStates
                                }
                            )
                            model.metadataForm
                }
                (TreeEdit.Ports.editing True)

        Delete fieldName ->
            let
                root = model.root
                selected = model.selected
            in
            case Selection.first selected of
                Just path ->
                    let
                        selectedNode = Tree.get path root
                    in
                    selectedNode
                      |> Lens.modify Tree.metadata (Dict.update (String.toUpper fieldName) (always Nothing))
                      |> (\newLeaf -> Tree.set path newLeaf root)
                      |> (\x -> { model | root = x })
                      |> R.succeed
                      |> R.handle model
                      |> message (Msg.Loaded <| Msg.Metadata NewSelection)

                Nothing ->
                    Return.singleton model

        Save ->
            case model.metadataForm |> Maybe.andThen (.form >> Form.getOutput) of
                Just metadata ->
                    save metadata model
                        |> message (Msg.Loaded <| Msg.Metadata NewSelection)
                        |> Return.command (TreeEdit.Ports.editing False)

                Nothing ->
                    Return.singleton model

        Cancel ->
            Return.singleton model
                |> message (Msg.Loaded <| Msg.Metadata NewSelection)
                |> Return.command (TreeEdit.Ports.editing False)

        -- TODO: why doesn't it work?
        -- (Cmd.batch [TreeEdit.Ports.editing False, Cmd.Extra.perform <| Msg.Metadata NewSelection])
        NewSelection ->
            let
                root = model.root
            in
            case Selection.getOne model.selected of
                Just p ->
                    let
                        node =
                            Tree.get p root

                        metadata =
                            .get Tree.metadata node
                    in
                    case Tree.isTerminal node of
                        True ->
                            let
                                lem =
                                    Dict.get "LEMMA" metadata

                                req x =
                                    Net.get
                                        (absolute ["dictentry"] [ string "lemma" x ])
                                        ReceivedDefinition
                                        D.string
                            in
                            Return.return
                                { model
                                    | metadataForm =
                                        Just <|
                                            init model.lemmata metadata node
                                }
                                (lem
                                    |> Maybe.map req
                                    |> Maybe.withDefault Cmd.none
                                    |> Cmd.map (Msg.Loaded << Msg.Metadata)
                                )

                        False ->
                            Return.singleton
                                { model
                                    | metadataForm =
                                        Just <| init model.lemmata metadata node
                                }

                _ ->
                    Return.singleton { model | metadataForm = Nothing }

        SaveSuccess lem ->
            Return.singleton model
              |> message (Msg.LogMessage <| "Saved definition for lemma " ++ lem)

        Key { keyCode } ->
            case keyCode of
                K.Escape ->
                    Return.return { model | metadataForm = Nothing } (TreeEdit.Ports.editing False)

                _ ->
                    Return.singleton model

        LemmaSelect submsg ->
            let
                -- State can be nothing in the case where we have used ESC to
                -- cancel editing the metadata entirely.  So we need to handle
                -- that case; otherwise this logic would be simpler
                state_ =
                    model.metadataForm |> Maybe.map .lemmaSelectState
            in
            case state_ of
                Just state ->
                    let
                        ( newState, cmd ) =
                            Select.update lemmaSelectConfig submsg state

                        newForm =
                            model.metadataForm |> Maybe.map (\x -> { x | lemmaSelectState = newState })
                    in
                    cmd
                      |> Cmd.map (Msg.Loaded << Msg.Metadata)
                      |> Return.return { model | metadataForm = newForm }

                Nothing ->
                    Return.singleton model


view : BigModel.ForestModel -> Html Msg
view model =
    case model.metadataForm of
        Just m ->
            div
                [ id "metadata-editor"
                , Attr.style "background-color" theme.offWhite2
                , Attr.style "padding-bottom" "2px"
                ]
                [ div
                    [ Attr.style "background-color" theme.darkGrey
                    , Attr.style "color" "white"
                    , Attr.style "width" "100%"
                    , Attr.style "height" "16px"
                    , Attr.style "font-weight" "bold"
                    , Attr.style "text-align" "center"
                    ]
                    [ text "Metadata" ]
                , formView m
                ]

        Nothing ->
            div [] []
