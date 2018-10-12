module TreeEdit.Metadata.Css exposing
    ( Style
    , editButton
    , metadataEditor
    , metadataInner
    , saveButtonContainer
    , textField
    , textFieldAbsent
    , textFieldEditBox
    , textFieldEditContainer
    , textFieldInner
    )

import Html exposing (Attribute)
import Html.Attributes exposing (style)

import TreeEdit.Metadata.Type exposing (Msg)

import TreeEdit.View.Theme exposing (theme)


type alias Style =
    List (Attribute Msg)


textField : Style
textField =
    [ style "border" <| "1px solid " ++ theme.darkGrey
    , style "margin" "2px"
    ]


textFieldInner : Style
textFieldInner =
    [ style "background-color" "rgb(85,85,85)"
    , style "color" "rgb(238,238,238)"
    , style "width" "100%"
    , style "height" "16px"
    , style "font-weight" "bold"
    , style "text-align" "center"
    ]


textFieldEditContainer : Style
textFieldEditContainer =
    [ style "display" "flex"
    , style "justify-content" "spaceBetween"
    , style "align-items" "center"
    , style "padding" "2px"
    ]


textFieldAbsent : Style
textFieldAbsent =
    [ style "color" "gray" ]


editButton : Style
editButton =
    [ style "padding" "2px 5px"
    , style "margin-left" "8px"
    , style "border-color" "#999999"
    , style "border-width" "1px"
    , style "border-radius" "5px"
    , style "border-style" "solid"
    ]


textFieldEditBox : Style
textFieldEditBox =
    [ style "width" "100%" ]


saveButtonContainer : Style
saveButtonContainer =
    [ style "margin" "2px"
    , style "display" "flex"
    , style "flex-direction" "row-reverse"
    ]


metadataEditor : Style
metadataEditor =
    [ style "background-color" theme.offWhite2
    , style "padding-bottom" "2px"
    ]


metadataInner : Style
metadataInner =
    [ style "background-color" theme.darkGrey
    , style "color" "white"
    , style "width" "100%"
    , style "height" "16px"
    , style "font-weight" "bold"
    , style "text-align" "center"
    ]
