module TreeEdit.View exposing ( view
                              , viewRootTree -- For memoization hack
                              , viewTree -- For static tree viewer
                              , ViewInfo -- ditto
                              )

import Array
import Dict
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Ev
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3)
import Json.Decode as Json
import RemoteData exposing (RemoteData(..))
import TreeEdit.Config exposing (Config)
import TreeEdit.ContextMenu as ContextMenu
import TreeEdit.Dialog as Dialog
import TreeEdit.Metadata as Metadata
import TreeEdit.Model as Model
import TreeEdit.Model.Type exposing (Model)
import TreeEdit.Msg as Msg exposing (Msg(..))
import TreeEdit.OrderedDict as OD
import TreeEdit.Path as Path exposing (Path)
import TreeEdit.Selection as Selection exposing (Selection)
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Tree, TreeInfo)
import TreeEdit.Tree.View exposing (labelString, terminalString)
import TreeEdit.View.Css as Css
import TreeEdit.View.LabelEdit as LabelEdit
import TreeEdit.View.LabelEdit.Type exposing (LabelForm)
import TreeEdit.View.ToolBar as ToolBar
import TreeEdit.View.Utils exposing (decodeMouse, onClick)


isIP : Config -> String -> Bool
isIP config label =
    List.any (\a -> String.startsWith a label) config.ipLabels

labelHtml : Tree -> Html Msg
labelHtml tree =
    let
        metadata =
            .get Tree.metadata tree

        hasCorrection =
            case metadata |> Dict.get "OLD-TAG" of
                Nothing ->
                    span [] []

                Just _ ->
                    span Css.correctionFlag [ text "CORR" ]

        hasError =
            case metadata |> Dict.get "VALIDATION-ERROR" of
                Nothing ->
                    span [] []

                Just _ ->
                    span Css.correctionFlag [ text "ERR" ]

        label =
            labelString tree
    in
    span [] [ text label, hasCorrection, hasError ]


type alias ViewInfo =
    { config : Config
    , selected : List Path
    , labelForm : Maybe LabelForm
    , interactive : Bool
    , snodeClass: String
    , ipClass: String
    , selectedClass: String
    , wnodeClass : String
    }


snode : ViewInfo -> Path -> Tree -> List (Html Msg) -> Html Msg
snode info self tree children =
    let
        selected =
            List.member self info.selected

        rightClick =
            Ev.custom "contextmenu" <|
                Json.map (\x -> { message = RightClick self x
                                , preventDefault = True
                                , stopPropagation = True
                                })
                    decodeMouse

        isIP_ =
            isIP info.config <| .get Tree.label tree

        label =
            case ( selected, info.labelForm ) of
                ( True, Just form ) ->
                    Html.map Msg.Label <| LabelEdit.view form

                _ ->
                    labelHtml tree
        eventHandlers = if info.interactive
                        then [ onClick <| ToggleSelect self , rightClick ]
                        else []
        cssClasses = Attr.classList
                     [ ( "snode", True )
                     , ( "selected", selected )
                     , ( "ip", not selected && isIP_ )
                     ]
    in
    div (cssClasses :: eventHandlers) <| label ::children

wnode : String -> Tree -> Html Msg
wnode className t =
    span
        [ Attr.class className ]
        [ text <| terminalString t ]


viewTree : ViewInfo -> Path -> Tree -> Html Msg
viewTree info selfPath tree =
    let
        childHtml =
            Tree.either
                (\_ -> [ wnode info.wnodeClass tree ])
                (\_ children ->
                    Array.indexedMap (\i c -> viewTree info (Path.childPath i selfPath) c) children
                        |> Array.toList
                )
                tree
    in
    snode info selfPath tree childHtml


viewRootTree : Config -> Maybe ( List Path, Maybe LabelForm ) -> String -> Tree -> Html Msg
viewRootTree config dataPack selfIndex tree =
    let
        _ = Debug.log "redraw" selfIndex
        selected = dataPack |> Maybe.map Tuple.first |> Maybe.withDefault []
        labelForm = dataPack |> Maybe.map Tuple.second |> Maybe.withDefault Nothing
        info = { config = config, selected = selected, labelForm = labelForm
               , interactive = True
               , snodeClass = "snode", selectedClass = "selected", ipClass = "ip", wnodeClass = "wnode"
               }
    in
    viewTree info (Path.singleton selfIndex) tree



-- Why do we go through all these contortions?  Html.lazy compares based on
-- equality of reference, not equality per se (source:
-- <https://groups.google.com/forum/#!topic/elm-discuss/AFnzfJtzfRA>).  Only
-- primitive types and a few others (e.g. Nothing, I think) are necesarily
-- referentially equal if they are also equal values.  The first problematic
-- case is the Path that we construct from the enumeration index, so we have a
-- special top-layer view function that takes an Int and constructs the path.
-- The second problem is the list of selected paths: we condense this to
-- Nothing if 1) nothing is selected or 2) the selection is not inside the
-- current tree.  There might be further performance optimizations we could
-- make here, but for now it's Good Enough™


viewRoot : Model -> (Maybe ( List Path, Maybe LabelForm ) -> String -> Tree -> Html Msg) -> List ( String, Html Msg )
viewRoot model vrt =
    let
        root = .get Model.root model
        selectedTrees = Selection.get model.selected
        selectedRoots = List.map Path.root selectedTrees
        labelForm = model.labelForm
        viewChild ( id, c ) =
            let
                data =
                    if List.member (Path.singleton id) selectedRoots
                    then Just ( selectedTrees, labelForm )
                    else Nothing
            in
            ( id, lazy3 vrt data id c )
    in
    root
      |> OD.toArray
      |> Array.map viewChild
      |> Array.toList


wrapSn0 : List ( String, Html Msg ) -> Html Msg
wrapSn0 nodes =
    let
        rightClick =
            Ev.custom "contextmenu" <|
                Json.map (\_ -> { message = RightClickRoot
                                , preventDefault = True
                                , stopPropagation = True
                                })
                    decodeMouse
    in
    nodes
        |> Keyed.node "div"
            [ Attr.id "sn0"
            , rightClick
            ]


view : Model -> Html Msg
view model =
    let
        loading =
            div [] [ text "loading" ]
    in
    case model.webdata of
        NotAsked ->
            loading

        Loading ->
            loading

        Failure e ->
            div [] [ text <| "error " ++ Debug.toString e ]

        Success { viewFn } ->
            div []
                [ model.dialog |> Maybe.map Dialog.view |> Maybe.withDefault (div [] [])
                , div Css.toolbar
                    [ ToolBar.view model.fileName
                    , Metadata.view model |> Html.map Msg.Metadata
                    ]
                , div Css.messages
                    [ div Css.titlebar [ text "Messages" ]
                    , text model.lastMessage
                    ]
                , viewRoot model viewFn |> wrapSn0
                , map Msg.Context <| ContextMenu.view model
                ]
