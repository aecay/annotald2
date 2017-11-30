module TreeEdit.ContextMenu.Css exposing (..)

import Css exposing (..)
import Html.Styled
import Html.Styled.Attributes exposing (css)

type alias Style msg = Html.Styled.Attribute msg

entry: Style msg
entry = css [ color (hex "#333333")
            , textDecoration none
            , lineHeight (px 20)
            , height (px 20)
            , padding2 (px 1) (px 5)
            , cursor pointer
            , hover [ backgroundColor (hex "#cccccc")]
            ]

heading : Style msg
heading = css [ color (hex "#FEEDD5")
              , backgroundColor (hex "#000000")
              , padding (px 2)
              , paddingLeft (px 5)
              , borderBottom3 (px 1) solid (hex "#c0c0c0")
              , borderLeft3 (px 1) solid (hex "#c0c0c0")
              , fontWeight bold
              ]

column : Style msg
column = css [ float left ]


contextMenu : Style msg
contextMenu = css [ position absolute
                  , zIndex (int 9999)
                  , border3 (px 1) solid (hex "#000000")
                  , backgroundColor (hex "#efefef")
                  , padding zero
                  , margin zero
                  ]
