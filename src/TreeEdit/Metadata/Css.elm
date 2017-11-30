module TreeEdit.Metadata.Css exposing (..)

import Css exposing (..)
import Css.Colors exposing (gray, white)
import Html.Styled
import Html.Styled.Attributes exposing (css)

import TreeEdit.View.Theme exposing (theme)

type alias Style msg = Html.Styled.Attribute msg

textField : Style msg
textField = css [ border3 (px 1) solid theme.darkGrey
                , margin (px 2)
                ]

textFieldInner : Style msg
textFieldInner = css [ backgroundColor (rgb 85 85 85)
                     , color (rgb 238 238 238)
                     , width (pct 100)
                     , height (px 16)
                     , fontWeight bold
                     , textAlign center
                     ]

textFieldEditContainer : Style msg
textFieldEditContainer = css [ displayFlex
                             , justifyContent spaceBetween
                             , alignItems center
                             , padding (px 2)
                             ]

textFieldAbsent : Style msg
textFieldAbsent = css [ color gray ]

editButton : Style msg
editButton = css [ padding (px 1) ]

textFieldEditBox : Style msg
textFieldEditBox = css [ width (pct 100) ]

saveButtonContainer : Style msg
saveButtonContainer = css [ margin (px 2)
                          , displayFlex
                          , flexDirection rowReverse
                          ]

metadataEditor : Style msg
metadataEditor = css [ backgroundColor theme.offWhite2
                     , paddingBottom (px 2)
                     ]

metadataInner : Style msg
metadataInner = css [ backgroundColor theme.darkGrey
                    , color white
                    , width (pct 100)
                    , height (px 16)
                    , fontWeight bold
                    , textAlign center
                    ]
