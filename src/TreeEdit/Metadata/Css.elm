module TreeEdit.Metadata.Css exposing (css, Classes(..))

import Css exposing (..)
import Css.Colors exposing (gray, white)
import Css.Namespace exposing (namespace)

import TreeEdit.View.Css exposing (ns, convertColor)
import TreeEdit.View.Theme exposing (theme)

type Classes = TextField | TextFieldInner | TextFieldEditContainer | TextFieldAbsent |
    EditButton | TextFieldEditBox | SaveButtonContainer | MetadataEditor | MetadataInner

css : Stylesheet
css = (stylesheet << namespace ns) <|
      [ class TextField [ border3 (px 1) solid <| convertColor theme.darkGrey
                        , margin (px 2)
                        ]
      , class TextFieldInner [ backgroundColor (rgb 85 85 85)
                             , color (rgb 238 238 238)
                             , width (pct 100)
                             , height (px 16)
                             , fontWeight bold
                             , textAlign center
                             ]
      , class TextFieldEditContainer [ displayFlex
                                     , justifyContent spaceBetween
                                     , alignItems center
                                     , padding (px 2)
                                     ]
      , class TextFieldAbsent [ color gray ]
      , class EditButton [ padding (px 1) ]
      , class TextFieldEditBox [ width (pct 100) ]
      -- TODO: could be ids instead...
      , class SaveButtonContainer [ margin (px 2)
                                  , displayFlex
                                  , flexDirection rowReverse
                                  ]
      , class MetadataEditor [ backgroundColor <| convertColor theme.offWhite2
                             , paddingBottom (px 2)
                             ]
      , class MetadataInner [ backgroundColor <| convertColor theme.darkGrey
                            , color white
                            , width (pct 100)
                            , height (px 16)
                            , fontWeight bold
                            , textAlign center
                            ]
      ]
