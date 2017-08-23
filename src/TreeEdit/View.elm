module TreeEdit.View exposing (view)

import Color exposing (black, rgb, Color)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Ev
import Html.Lazy exposing (lazy3)
import Json.Decode as Json
import RemoteData exposing (RemoteData(..))
import Toolkit.Helpers exposing (applyList)
import TypedStyles exposing ( borderTopWidth, borderTopColor, borderBottomWidth, borderBottomColor
                            , border, solid, borderLeftColor, borderLeftWidth
                            , padding , color , px , backgroundColor , marginLeft
                            )

import TreeEdit.Model as Model exposing (Model)
import TreeEdit.Tree as Tree exposing (Tree)
import TreeEdit.Tree.View exposing (labelString, terminalString)
import TreeEdit.Path as Path exposing (Path)
import TreeEdit.Selection as Selection exposing (Selection)
import TreeEdit.Msg as Msg exposing (Msg(..))
import TreeEdit.View.ToolBar as ToolBar
import TreeEdit.Metadata as Metadata

import TreeEdit.Utils as Utils exposing (fromJust)
import TreeEdit.ViewUtils exposing (onClick, blockAll)

import TreeEdit.ContextMenuTypes as ContextMenuTypes
import TreeEdit.ContextMenu as ContextMenu

-- TODO: use html.keyed for speed

blockAll : Ev.Options
blockAll = { stopPropagation = True
           , preventDefault = True
           }

decodeMouse : Json.Decoder ContextMenuTypes.Position
decodeMouse = Json.map2 (\x y -> { x = x, y = y })
              (Json.field "x" Json.int)
              (Json.field "y" Json.int)

ipStyles : List (String, String)
ipStyles = [ borderTopColor black
           , borderTopWidth 1 px
           , borderBottomColor black
           , borderBottomWidth 1 px
           ]

isIP : String -> Bool
isIP label =
    let
        predicates = List.map String.startsWith ["IP-", "FRAG"] -- TODO: make configurable
    in
        List.any identity <| applyList predicates label

theme : { offWhite : Color, salmon : Color, silver : Color, blue : Color }
theme = { blue = rgb 70 130 180
        , salmon = rgb 197 144 142
        , offWhite = rgb 239 239 239
        , silver = (rgb 192 192 192)
        }

snode : Path -> Tree -> Bool -> List (Html Msg) -> Html Msg
snode self tree selected children =
    let
        rightClick = Ev.onWithOptions "contextmenu" blockAll <|
                     Json.map (\x -> RightClick self x) decodeMouse
        isIP_ = isIP tree.label
        bgColor = if selected
                  then theme.blue
                  else if isIP_
                       then theme.salmon
                       else theme.offWhite
    in
        div
        [ Attr.class "snode"
        , Attr.style <| [ marginLeft 20 px
                        , border 1 px solid theme.silver
                        , borderLeftColor theme.blue
                        , borderLeftWidth 4 px
                        , padding 2 px
                        , color black
                        , backgroundColor bgColor
                        , ("cursor", "pointer")
                        ] ++ if isIP_ then ipStyles else []
        , onClick <| ToggleSelect self
        , rightClick
        ] <| text (labelString tree) :: children

wnode : Tree -> Html Msg
wnode t = span
             [ Attr.class "wnode"
             , Attr.style [ ("margin-left", "20px")
                          , ("padding-left", "4px")
                          , ("padding-right", "4px")
                          , ("border", "1px solid black")
                          , ("background-color", "white")
                          , ("color", "black")
                          ]
             ]
             [text <| terminalString t]

viewTree : List Path -> Path -> Tree -> Html Msg
viewTree selected selfPath tree =
    let
        isSelected = List.member selfPath selected
        viewT d = snode selfPath d isSelected [wnode d]
        viewNt d =
            (.getOption Tree.children) tree |> Utils.fromJust |>
            List.indexedMap (\i c -> viewTree selected (Path.childPath i selfPath) c) |>
            snode selfPath d isSelected
    in
        Tree.either viewNt viewT tree

viewRootTree : Maybe (List Path) -> Int -> Tree -> Html Msg
viewRootTree selected selfIndex tree = viewTree (Maybe.withDefault [] selected) (Path.singleton selfIndex) tree

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
viewRoot : Model -> List (Html Msg)
viewRoot model =
    let
        selectedTrees1 = Selection.get model.selected
        selectedTrees = if selectedTrees1 == [] then Nothing else Just selectedTrees1
    in
        (.get Model.root model) |>
        -- Possibly can make this a Lens, if the children of a terminal are
        -- defined as []
        (.getOption Tree.children) |>
        Utils.fromJust |>
        List.indexedMap (\i c ->
                             let
                                 sel = if Just True == Maybe.map
                                       (\x -> List.member (Path.singleton i) (List.map Path.root x))
                                       selectedTrees
                                       then selectedTrees
                                       else Nothing
                             in
                                 lazy3 viewRootTree sel i c)

-- TODO: lame name
viewRoot1 : Model -> Html Msg
viewRoot1 m =
    let
        rightClick = Ev.onWithOptions "contextmenu" blockAll <|
                     Json.map (\_ -> RightClickRoot) decodeMouse
    in
        viewRoot m |>
        div [ Attr.class "sn0"
            , Attr.style [ ("background-color", "#D2B48C")
                         , ("border", "1px solid black")
                         , ("margin-left", "calc(15% + 12px)")
                         , ("margin-right", "5%")
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
        case model.root of
            NotAsked -> loading
            Loading -> loading
            Failure e -> div [] [ text <| "error " ++ toString e ]
            Success root -> div [] [ div [ Attr.style [ ("position", "fixed")
                                                      , ("top", "30px")
                                                      , ("left", "0px")
                                                      , ("margin-left", "5px")
                                                      , ("width", "15%")
                                                      ]

                                         ]
                                         [ ToolBar.view model.fileName
                                         , Metadata.view model |> Html.map Msg.Metadata
                                         ]
                                   , div [ Attr.style [ ("position", "fixed")
                                                      , ("bottom", "30px")
                                                      , ("left", "0px")
                                                      , ("margin-left", "5px")
                                                      , ("width", "15%")
                                                      , ("background", "#FEF6EA")
                                                      ]
                                         ]
                                         [ div [ Attr.style [ ("background-color", "#2E2E2E")
                                                            , ("color", "white")
                                                            , ("font-weight", "bold")
                                                            , ("text-align", "center")
                                                            , ("width", "100%")
                                                            , ("height", "16px")
                                                            ]
                                               ] [ text "Messages" ]
                                         , text model.lastMessage ]
                                   , viewRoot1 model
                                   , map Msg.Context <| ContextMenu.view model Model.contextMenu
                                   ]
