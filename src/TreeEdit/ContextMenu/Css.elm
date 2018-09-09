module TreeEdit.ContextMenu.Css exposing (Style, column, contextMenu, entry, heading)

import Html exposing (Attribute)
import Html.Attributes exposing (style)

import TreeEdit.ContextMenu.Type exposing (Msg)

type alias Style =
    List (Attribute Msg)


entry : Style
entry =
    [ style "color" "#333333"
    , style "text-decoration" "none"
    , style "line-height" "20px"
    , style "height" "20px"
    , style "padding" "1px 5px"
    , style "cursor" "pointer"

    -- TODO , "hover" [ backgroundColor (hex "#cccccc")]
    ]


heading : Style
heading =
    [ style "color" "#FEEDD5"
    , style "background-color" "black"
    , style "padding" "2px"
    , style "padding-left" "5px"
    , style "border-bottom" "1px solid #c0c0c0"
    , style "border-left" "1px solid #c0c0c0"
    , style "font-weight" "bold"
    ]


column : Style
column =
    [ style "float" "left" ]


contextMenu : Style
contextMenu =
    [ style "position" "absolute"
    , style "z-index" "9999"
    , style "border" "1px solid black"
    , style "background-color" "#efefef"
    , style "padding" "0px"
    , style "margin" "0px"
    ]
