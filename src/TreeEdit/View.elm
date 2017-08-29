module TreeEdit.View exposing ( view
                              , viewRootTree -- For memoization hack
                              )

import Color exposing (black, white, Color)
import Guards exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Ev
import Html.Lazy exposing (lazy3)
import Json.Decode as Json
import RemoteData exposing (RemoteData(..))
import Toolkit.Helpers exposing (applyList)
import TypedStyles exposing ( borderTopWidth, borderTopColor, borderBottomWidth, borderBottomColor
                            , border, solid, borderLeftColor, borderLeftWidth
                            , padding, color, px, backgroundColor, marginLeft
                            , paddingLeft, paddingRight, width, height, prc
                            , left, right, bottom, marginRight, top, textCenter
                            )

import TreeEdit.Config exposing (Config)
import TreeEdit.Model.Type exposing (Model)
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Tree.View exposing (labelString, terminalString)
import TreeEdit.Path as Path exposing (Path)
import TreeEdit.Selection as Selection exposing (Selection)
import TreeEdit.Msg as Msg exposing (Msg(..))
import TreeEdit.View.ToolBar as ToolBar
import TreeEdit.View.Theme exposing (theme)
import TreeEdit.Metadata as Metadata
import TreeEdit.View.LabelEdit as LabelEdit
import TreeEdit.View.LabelEdit.Type exposing (LabelForm)

import TreeEdit.Utils as Utils exposing (fromJust)
import TreeEdit.ViewUtils exposing (onClick, blockAll)

import TreeEdit.ContextMenuTypes as ContextMenuTypes
import TreeEdit.ContextMenu as ContextMenu

blockAll : Ev.Options
blockAll = { stopPropagation = True
           , preventDefault = True
           }

decodeMouse : Json.Decoder ContextMenuTypes.Position
decodeMouse = Json.map2 (\x y -> { x = x, y = y })
              (Json.field "pageX" Json.int)
              (Json.field "pageY" Json.int)

ipStyles : List (String, String)
ipStyles = [ borderTopColor black
           , borderTopWidth 1 px
           , borderBottomColor black
           , borderBottomWidth 1 px
           ]

isIP : Config -> String -> Bool
isIP config label =
    let
        predicates = List.map String.startsWith config.ipLabels
    in
        List.any identity <| applyList predicates label

bgColor : Bool -> Bool -> Color
bgColor selected ip = selected => theme.blue
                      |= ip => theme.salmon
                      |= theme.offWhite

type alias ViewInfo =
    { config : Config
    , selected : List Path
    , labelForm : Maybe LabelForm
    }

snode : ViewInfo -> Path -> Tree -> List (Html Msg) -> Html Msg
snode info self tree children =
    let
        selected = List.member self info.selected
        rightClick = Ev.onWithOptions "contextmenu" blockAll <|
                     Json.map (\x -> RightClick self x) decodeMouse
        isIP_ = isIP info.config tree.label
        label = case (selected, info.labelForm) of
                    (True, Just form) -> Html.map Msg.Label <| LabelEdit.view form
                    _ -> text <| labelString tree
    in
        div
        [ Attr.class "snode"
        , Attr.style <| [ marginLeft 20 px
                        , border 1 px solid theme.silver
                        , borderLeftColor theme.blue
                        , borderLeftWidth 4 px
                        , padding 2 px
                        , color black
                        , backgroundColor (bgColor selected isIP_)
                        , ("cursor", "pointer")
                        ] ++ if isIP_ then ipStyles else []
        , onClick <| ToggleSelect self
        , rightClick
        ] <| label :: children

wnode : Tree -> Html Msg
wnode t = span
             [ Attr.class "wnode"
             , Attr.style [ marginLeft 20 px
                          , paddingLeft 4 px
                          , paddingRight 4 px
                          , border 1 px solid black
                          , backgroundColor white
                          , color black
                          ]
             ]
             [text <| terminalString t]

viewTree : ViewInfo -> Path -> Tree -> Html Msg
viewTree info selfPath tree =
    let
        isSelected = List.member selfPath info.selected
        viewT d = snode info selfPath d [wnode d]
        viewNt d =
            (.getOption Tree.children) tree |> Utils.fromJust |>
            List.indexedMap (\i c -> viewTree info (Path.childPath i selfPath) c) |>
            snode info selfPath d
    in
        Tree.either viewNt viewT tree

viewRootTree : Config -> Maybe (List Path, Maybe LabelForm) -> Int -> Tree -> Html Msg
viewRootTree config dataPack selfIndex tree =
    let
        _ = Debug.log "redraw" (dataPack, selfIndex)
        selected = dataPack |> Maybe.map Tuple.first |> Maybe.withDefault []
        labelForm = dataPack |> Maybe.map Tuple.second |> Maybe.withDefault Nothing
        info = { config = config, selected = selected, labelForm = labelForm }
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
viewRoot : Model -> Tree -> Config -> List (Html Msg)
viewRoot model root config =
    let
        selectedTrees = Selection.get model.selected
        selectedRoots = (List.map Path.root selectedTrees)
        labelForm = model.labelForm
        vrt = model.viewRootWithConfig |> Maybe.withDefault (viewRootTree config)
    in
        root |>
        -- Possibly can make this a Lens, if the children of a terminal are
        -- defined as []
        (.getOption Tree.children) |>
        Utils.fromJust |>
        List.indexedMap (\i c ->
                             let
                                 data = if List.member (Path.singleton i) selectedRoots
                                        then Just (selectedTrees, labelForm)
                                        else Nothing
                             in
                                 lazy3 vrt data i c)

wrapSn0 : List (Html Msg) -> Html Msg
wrapSn0 nodes =
    let
        rightClick = Ev.onWithOptions "contextmenu" blockAll <|
                     Json.map (\_ -> RightClickRoot) decodeMouse
    in
        nodes |>
        div [ Attr.class "sn0"
            , Attr.style [ backgroundColor theme.tan
                         , border 1 px solid black
                         , marginRight 5 prc
                         , ("margin-left", "calc(15% + 12px)")
                         -- In order for the trees to shrink to the correct width
                         , ("display", "inline-block")
                         ]
            , rightClick
            ]

view : Model -> Html Msg
view model =
    let
        loading = div [] [ text "loading" ]
    in
        case model.webdata of
            NotAsked -> loading
            Loading -> loading
            Failure e -> div [] [ text <| "error " ++ toString e ]
            Success (root, config) -> div [] [ div [ Attr.style [ top 30 px
                                                                , left 0 px
                                                                , marginLeft 5 px
                                                                , width 15 prc
                                                                , ("position", "fixed")
                                                                ]

                                                   ]
                                                   [ ToolBar.view model.fileName
                                                   , Metadata.view model |> Html.map Msg.Metadata
                                                   ]
                                             , div [ Attr.style [ bottom 30 px
                                                                , left 0 px
                                                                , marginLeft 5 px
                                                                , width 15 prc
                                                                , backgroundColor theme.offWhite2
                                                                , ("position", "fixed")
                                                                ]
                                                   ]
                                                   [ div [ Attr.style [ backgroundColor theme.darkGrey
                                                                      , color white
                                                                      , width 100 prc
                                                                      , height 16 px
                                                                      , ("font-weight", "bold")
                                                                      , textCenter
                                                                      ]
                                                         ] [ text "Messages" ]
                                                   , text model.lastMessage ]
                                             , viewRoot model root config |> wrapSn0
                                             , map Msg.Context <| ContextMenu.view model
                                             ]
