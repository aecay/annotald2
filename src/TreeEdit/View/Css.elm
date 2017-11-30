module TreeEdit.View.Css exposing (..)

import Html.Styled
import Html.Styled.Attributes exposing (css)

import Css exposing (..)
import Css.Colors exposing (black, white)

import TreeEdit.Msg exposing (Msg)
import TreeEdit.View.Theme exposing (theme)

type alias Style = Html.Styled.Attribute Msg

ip : Css.Style
ip = Css.batch [ borderTopColor black
               , borderTopWidth (px 1)
               , borderBottomColor black
               , borderBottomWidth (px 1)
               , backgroundColor theme.salmon
               ]

selected : Css.Style
selected = Css.batch [ backgroundColor theme.blue ]

snode : Css.Style
snode = Css.batch [ marginLeft (px 20)
                  , border3 (px 1) solid theme.silver
                  , borderLeftColor theme.blue
                  , borderLeftWidth (px 4)
                  , padding (px 2)
                  , color black
                  , backgroundColor theme.offWhite
                  , cursor pointer
                  ]

toolbar : Style
toolbar = css [ top (px 30)
              , left (px 0)
              , marginLeft (px 5)
              , width (pct 15)
              , position fixed
              ]

messages : Style
messages = css [ bottom (px 30)
               , left (px 0)
               , marginLeft (px 5)
               , width (pct 15)
               , backgroundColor theme.offWhite2
               , position fixed
               ]

titlebar : Style
titlebar = css [ backgroundColor theme.darkGrey
               , color white
               , width (pct 100)
               , height (px 16)
               , fontWeight bold
               , textAlign center
               ]

sn0 : Style
sn0 = css [ backgroundColor theme.tan
          , border3 (px 1) solid black
          , marginRight (pct 5)
          , marginLeft <| calc (pct 15) plus (px 12)
          -- In order for the trees to shrink to the correct width
          , display inlineBlock
          ]

rootSnode : Css.Style
rootSnode = Css.batch [ border3 (px 2) solid black ]

wnode : Style
wnode = css [ marginLeft (px 20)
            , paddingLeft (px 4)
            , paddingRight (px 4)
            , border3 (px 1) solid black
            , backgroundColor white
            , color black
            ]

correctionFlag : Style
correctionFlag = css [ border3 (px 1) solid (hex "C17900")
                     , backgroundColor (hex "FCD271")
                     , color black
                     , fontSize (px 6)
                     , padding (px 1)
                     , verticalAlign textTop
                     ]
