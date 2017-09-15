module TreeEdit.ContextMenu.Css exposing (css, Classes(..), Ids(..))

import Css exposing (..)
import Css.Namespace exposing (namespace)

import TreeEdit.View.Css exposing (ns)

type Classes = Entry | Heading | Column
type Ids = ContextMenu

css : Stylesheet
css = (stylesheet << namespace ns) <|
      [ class Entry [ color (hex "#333333")
                    , textDecoration none
                    , lineHeight (px 20)
                    , height (px 20)
                    , padding2 (px 1) (px 5)
                    , cursor pointer
                    , hover [ backgroundColor (hex "#cccccc")]
                    ]
      , class Heading [ color (hex "#FEEDD5")
                      , backgroundColor (hex "#000000")
                      , padding (px 2)
                      , paddingLeft (px 5)
                      , borderBottom3 (px 1) solid (hex "#c0c0c0")
                      , borderLeft3 (px 1) solid (hex "#c0c0c0")
                      , fontWeight bold
                      ]
      , class Column [ float left ]
      , id ContextMenu [ position absolute
                       , zIndex (int 9999)
                       , border3 (px 1) solid (hex "#000000")
                       , backgroundColor (hex "#efefef")
                       , padding zero
                       , margin zero
                       ]
      ]
