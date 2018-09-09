module TreeEdit.View.Css exposing (Style, correctionFlag, messages, titlebar, toolbar)

import Html exposing (Attribute)
import Html.Attributes exposing (style)

import TreeEdit.Msg exposing (Msg)
import TreeEdit.View.Theme exposing (theme)


type alias Style =
    List (Attribute Msg)



-- ip : Style
-- ip = [ ("border-top-color", "black")
--      , ("border-top-width", "1px")
--      , ("border-bottom-color", "black")
--      , ("border-bottom-width", "1px")
--      , ("background-color", theme.salmon)
--      ]
-- selected : Style
-- selected = [ ("background-color", theme.blue) ]
-- snode : Style
-- snode = [ ("margin-left", "20px")
--         , ("border3", "1px solid " ++ theme.silver)
--         , ("border-left-color", theme.blue)
--         , ("border-left-width", "4px")
--         , ("padding", "2px")
--         , ("color", "black")
--         , ("background-color", theme.offWhite)
--         , ("cursor", "pointer")
--         ]


toolbar : Style
toolbar =
    [ style "top" "30px"
    , style "left" "0px"
    , style "margin-left" "5px"
    , style "width" "15%"
    , style "position" "fixed"
    ]


messages : Style
messages =
    [ style "bottom" "30px"
    , style "left" "0px"
    , style "margin-left" "5px"
    , style "width" "15%"
    , style "background-color" theme.offWhite2
    , style "position" "fixed"
    ]


titlebar : Style
titlebar =
    [ style "background-color" theme.darkGrey
    , style "color" "white"
    , style "width" "100%"
    , style "height" "16px"
    , style "font-weight" "bold"
    , style "text-align" "center"
    ]



-- sn0 : Style
-- sn0 = [ ("background-color", theme.tan)
--       , ("border", "1px solid black")
--       , ("margin-right", "5%")
--       , ("margin-left", "calc(15% + 12px)")
--       -- In order for the trees to shrink to the correct width
--       , ("display", "inline-block")
--       ]
-- rootSnode : Style
-- rootSnode = [ ("border", "2px solid black") ]
-- wnode : Style
-- wnode = [ ("margin-left", "20px")
--         , ("padding-left", "4px")
--         , ("padding-right", "4px")
--         , ("border", "1px solid black")
--         , ("background-color", "white")
--         , ("color", "black")
--         ]


correctionFlag : Style
correctionFlag =
    [ style "border" "1px solid #C17900"
    , style "background-color" "#FCD271"
    , style "color" "black"
    , style "fontSize" "6px"
    , style "padding" "1px"
    , style "verticalAlign" "text-top"
    ]
