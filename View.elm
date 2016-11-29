module View exposing (view)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Ev
-- import Html.Lazy as L
import Json.Decode as Json

import Model exposing (Model)
import Tree exposing (Tree, TreeZipper, either, IndexVariety(..), Index, TreeDatum)
import Selection
import Update exposing (Msg(..))

import Utils exposing (fromJust, (?>))

import ZipperExts as ZX
-- import MultiwayTreeZipper as Z

-- TODO: use html.keyed for speed

indexString : Index -> String
indexString {number, variety} =
    let
        vstr = case variety of
                   Normal -> "-"
                   Gap -> "+"
    in toString number ++ vstr

labelText : TreeDatum -> String
labelText {label, index} =
    index ?>
    indexString |>
    Maybe.withDefault "" |>
    flip (++) label


snode : TreeZipper -> TreeDatum -> Bool -> List (Html Msg) -> Html Msg
snode zipper datum selected children =
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
    , Ev.onWithOptions "click" { stopPropagation = True
                               , preventDefault = True
                               } <|
        Json.succeed <| ToggleSelect zipper
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

viewTree : List TreeZipper -> TreeZipper -> Html Msg
viewTree selected zipper =
    let
        isSelected = List.member zipper selected
        viewT d =
            snode zipper d isSelected [wnode (fromJust d.text)]
        viewNt d =
            -- TODO: I suspect this of being a performance hotspot.  We can
            -- use Html.lazy here, but not above
            snode zipper d isSelected <|
                List.map (viewTree selected) <| ZX.childZippers zipper
    in
        either viewNt viewT zipper

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
        selectedTrees = Selection.get model.selected
    in
        model.root |>
        ZX.zipper |>
        ZX.childZippers |>
        List.map (viewTree selectedTrees)

view : Model -> Html Msg
view m =
    viewRoot m |>
    div [ Attr.class "sn0"
        , Attr.style [ ("background-color", "#D2B48C")
                     , ("border", "1px solid black")
                     , ("margin-left", "10%")
                     , ("margin-right", "5%")
                     , ("display", "inline-block")
                     ]
        ]
