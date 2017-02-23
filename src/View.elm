module View exposing (view)

import Html exposing (..)
import Html.Attributes as Attr
-- import Html.Lazy as L
import Json.Decode as Json
import Html.Events as Ev

import Model exposing (Model)
import Tree exposing (Tree)
import Path exposing (Path)
import Selection exposing (Selection)
import Msg exposing (Msg(..))
import Index

import Utils exposing (fromJust)
import ViewUtils exposing (onClick, blockAll)

import ContextMenuTypes
import ContextMenu

-- TODO: use html.keyed for speed

-- XXX: Seems to belong better in Tree or Index, but it's really a view
-- function (about presentation) so it lives here.  Of course, so is
-- terminalString, but that needs to know about internal types of Tree, so it
-- lives there.
labelText : Tree -> String
labelText tree =
    let
        index = (.getOption Tree.index) tree
        label = tree.label
    in
        index |>
        Maybe.map Index.string |>
        Maybe.withDefault "" |>
        (++) label

blockAll : Ev.Options
blockAll = { stopPropagation = True
           , preventDefault = True
           }

decodeMouse : Json.Decoder ContextMenuTypes.Position
decodeMouse = Json.map2 (\x y -> { x = x, y = y })
              (Json.field "x" Json.int)
              (Json.field "y" Json.int)

snode : Path -> Tree -> Bool -> List (Html Msg) -> Html Msg
snode self tree selected children =
    let
        rightClick = Ev.onWithOptions "contextmenu" blockAll <|
                     Json.map (\x -> RightClick self x) decodeMouse
    in
        div
        [ Attr.class "snode"
        , Attr.style [ ("margin-left", "20px")
                     , ("border", "1px solid silver")
                     , ("border-left", "4px solid #4682B4")
                     , ("background-color", if selected
                                            then "#4682B4"
                                            else "#EFEFEF")
                     , ("padding", "2px")
                     , ("cursor", "pointer")
                     , ("color", "black")
                     ]
        , onClick <| ToggleSelect self
        , rightClick
        ] <| text (labelText tree) :: children

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
             [text <| Tree.terminalString <| t]

viewTree : Selection -> Path -> Tree -> Html Msg
viewTree selected selfPath tree =
    let
        isSelected = List.member selfPath (Selection.get selected)
        viewT d =
            snode selfPath d isSelected [wnode d]
        viewNt d =
            -- TODO: I suspect this of being a performance hotspot.  We can
            -- use Html.lazy here, but not above
            Tree.children tree |>
            Utils.enumerate |>
            List.map (\(i, c) -> viewTree selected (Path.childPath i selfPath) c) |>
            snode selfPath d isSelected
    in
        Tree.either viewNt viewT tree

viewRoot : Model -> List (Html Msg)
viewRoot model =
    let
        selectedTrees = model.selected
    in
        model.root |>
        Tree.children |>
        Utils.enumerate |>
        List.map (\(i, c) -> viewTree selectedTrees (Path.singleton i) c)

-- TODO: ame name
viewRoot1 : Model -> Html Msg
viewRoot1 m =
    viewRoot m |>
    div [ Attr.class "sn0"
        , Attr.style [ ("background-color", "#D2B48C")
                     , ("border", "1px solid black")
                     , ("margin-left", "10%")
                     , ("margin-right", "5%")
                     , ("display", "inline-block")
                     ]
        ]

view : Model -> Html Msg
view model =
    div [] [ viewRoot1 model
           , map Msg.Context <| ContextMenu.view model Model.contextMenu
           ]
