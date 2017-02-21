module View exposing (view)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Ev
-- import Html.Lazy as L
import Json.Decode as Json

import Model exposing (Model)
import Tree exposing (Tree, TreeDatum, either)
import Path exposing (Path)
import MultiwayTree as T
import Selection exposing (Selection)
import Msg exposing (Msg(..))
import Index

import Utils exposing (fromJust)
import ViewUtils exposing (onClick, blockAll)

import ContextMenu

-- TODO: use html.keyed for speed

labelText : TreeDatum -> String
labelText {label, index} =
    index |>
    Maybe.map Index.string |>
    Maybe.withDefault "" |>
    (++) label

blockAll : Ev.Options
blockAll = { stopPropagation = True
           , preventDefault = True
           }

decodeMouse : Json.Decoder ContextMenu.Position
decodeMouse = Json.map2 (\x y -> { x = x, y = y })
              (Json.field "x" Json.int)
              (Json.field "y" Json.int)

snode : Path -> TreeDatum -> Bool -> List (Html Msg) -> Html Msg
snode self datum selected children =
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
    , Ev.onWithOptions "contextmenu" blockAll (Json.map (\x -> RightClick self x) decodeMouse)
    ] <| text (labelText datum) :: children

wnode : String -> Html Msg
wnode txt = span
             [ Attr.class "wnode"
             , Attr.style [ ("margin-left", "20px")
                          , ("padding-left", "4px")
                          , ("padding-right", "4px")
                          , ("border", "1px solid black")
                          , ("background-color", "white")
                          , ("color", "black")
                          ]
             ]
             [text txt]

viewTree : Selection -> Path -> Tree -> Html Msg
viewTree selected selfPath tree =
    let
        d = T.datum tree
        isSelected = List.member selfPath (Selection.get selected)
        viewT d =
            snode selfPath d isSelected [wnode (fromJust d.text)]
        viewNt d =
            -- TODO: I suspect this of being a performance hotspot.  We can
            -- use Html.lazy here, but not above
            T.children tree |>
            Utils.enumerate |>
            List.map (\(i, c) -> viewTree selected (Path.childPath i selfPath) c) |>
            snode selfPath d isSelected
    in
        either viewNt viewT tree

-- viewTree1 : Maybe TreeZipper -> TreeZipper -> Html Msg
-- viewTree1 selected target =
--     let isSelected =
--         case selected of
--             Nothing -> False
--             Just z -> target == z
--     in
--         L.lazy2 viewTree target isSelected

viewRoot : Model -> List (Html Msg)
viewRoot model =
    let
        selectedTrees = model.selected
    in
        model.root |>
        T.children |>
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
