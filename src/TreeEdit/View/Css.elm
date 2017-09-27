module TreeEdit.View.Css exposing (Classes(..), Ids(..), css, ns, convertColor)

import Color exposing (toRgb)
import Css exposing (..)
import Css.Colors exposing (black, white)
import Css.Namespace exposing (namespace)

import TreeEdit.View.Theme exposing (theme)

type Classes = Snode | Titlebar | Wnode | SnodeIp | SnodeSelected | CorrectionFlag
type Ids = Toolbar | ContextMenu | Messages | Sn0

convertColor : Color.Color -> Css.Color
convertColor c =
    let
        {red, green, blue} = toRgb c
    in
        Css.rgb red green blue

ns : String
ns = "annotald"

ip : Style
ip = batch [ borderTopColor black
           , borderTopWidth (px 1)
           , borderBottomColor black
           , borderBottomWidth (px 1)
           , backgroundColor (convertColor theme.salmon)
           ]

selected : Style
selected = backgroundColor (convertColor theme.blue)

snode : Style
snode = batch [ marginLeft (px 20)
              , border3 (px 1) solid (convertColor theme.silver)
              , borderLeftColor (convertColor theme.blue)
              , borderLeftWidth (px 4)
              , padding (px 2)
              , color black
              , backgroundColor (convertColor theme.offWhite)
              , cursor pointer
              ]

css : Stylesheet
css = (stylesheet << namespace ns) <|
      [ id Toolbar [ top (px 30)
                   , left (px 0)
                   , marginLeft (px 5)
                   , width (pct 15)
                   , position fixed
                   ]
      , id Messages [ bottom (px 30)
                    , left (px 0)
                    , marginLeft (px 5)
                    , width (pct 15)
                    , backgroundColor (convertColor theme.offWhite2)
                    , position fixed
                    ]
      , class Titlebar [ backgroundColor (convertColor theme.darkGrey)
                       , color white
                       , width (pct 100)
                       , height (px 16)
                       , fontWeight bold
                       , textAlign center
                       ]
      , id Sn0 [ backgroundColor (convertColor theme.tan)
               , border3 (px 1) solid black
               , marginRight (pct 5)
               , marginLeft <| calc (pct 15) plus (px 12)
               -- In order for the trees to shrink to the correct width
               , display inlineBlock
               , children [ class Snode [ border3 (px 2) solid black ]
                          , class SnodeIp [ border3 (px 2) solid black ]
                          , class SnodeSelected [ border3 (px 2) solid black ]
                          ]
               ]
      , class Wnode [ marginLeft (px 20)
                    , paddingLeft (px 4)
                    , paddingRight (px 4)
                    , border3 (px 1) solid black
                    , backgroundColor white
                    , color black
                    ]
      , class Snode [snode]
      , class SnodeIp [snode, ip]
      , class SnodeSelected [snode, selected]
      , class CorrectionFlag [ border3 (px 1) solid (hex "C17900")
                             , backgroundColor (hex "FCD271")
                             , color black
                             , fontSize (px 6)
                             , padding (px 1)
                             , verticalAlign textTop
                             ]
      ]
