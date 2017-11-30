module TreeEdit.Dialog exposing (Dialog(..), view)

import Html exposing (div, textarea, text, button, Html)
import Html.Attributes as Attr
import Html.Events as E

import Css exposing ( zIndex, color, rgba, rgb, width, height, position
                    , fixed, left, top, pct, opacity, backgroundColor
                    , int, num
                    )
import Html.CssHelpers

import TreeEdit.Msg exposing (Msg(DismissDialog))
import TreeEdit.View.Css as ViewCss exposing (Classes(Titlebar))

type Dialog = Copy String

styles : List Css.Style -> Html.Attribute msg
styles = Css.asPairs >> Attr.style

{class} = Html.CssHelpers.withNamespace ViewCss.ns

view : Dialog -> Html Msg
view dialog =
    let
        title = case dialog of
                    Copy _ -> "Copy tree"
        view = case dialog of
                   Copy txt -> div [] [ textarea [ Attr.id "copySource", Attr.readonly True ] [text txt]
                                      , button [ Attr.id "copyButton"
                                               , E.onClick DismissDialog
                                               , Attr.attribute "data-clipboard-target" "#copySource"
                                               ]
                                            [text "Copy"]
                                      ]
    in
        div [ styles [ zIndex (int 998)
                     , color (rgba 0 0 0 0.25)
                     , width (pct 100)
                     , height (pct 100)
                     , position fixed
                     , left (pct 0)
                     , top (pct 0)
                     ]
            ]
        [ div [ styles [ position fixed
                       , top (pct 25)
                       , left (pct 25)
                       , width (pct 50)
                       , height (pct 50)
                       , zIndex (int 999)
                       , opacity (num 0.85)
                       , backgroundColor (rgb 210 180 140)
                       ]
              ]
              [ div [ class [Titlebar] ] [text title]
              , view
              ]
        ]
