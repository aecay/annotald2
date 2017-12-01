module TreeEdit.Metadata.Css exposing (..)

import TreeEdit.View.Theme exposing (theme)

type alias Style = List (String, String)

textField : Style
textField = [ ("border", "1px solid " ++ theme.darkGrey)
            , ("margin", "2px")
            ]

textFieldInner : Style
textFieldInner = [ ("background-color", "rgb(85,85,85)")
                 , ("color", "rgb(238,238,238)")
                 , ("width", "100%")
                 , ("height", "16px")
                 , ("font-weight", "bold")
                 , ("text-align", "center")
                 ]

textFieldEditContainer : Style
textFieldEditContainer = [ ("display", "flex")
                         , ("justify-content", "spaceBetween")
                         , ("align-items", "center")
                         , ("padding", "2px")
                         ]

textFieldAbsent : Style
textFieldAbsent = [ ("color", "gray") ]

editButton : Style
editButton = [ ("padding", "1px") ]

textFieldEditBox : Style
textFieldEditBox = [ ("width", "100%") ]

saveButtonContainer : Style
saveButtonContainer = [ ("margin", "2px")
                      , ("display", "flex")
                      , ("flex-direction", "row-reverse")
                      ]

metadataEditor : Style
metadataEditor = [ ("background-color", theme.offWhite2)
                 , ("padding-bottom", "2px")
                 ]

metadataInner : Style
metadataInner = [ ("background-color", theme.darkGrey)
                , ("color", "white")
                , ("width", "100%")
                , ("height", "16px")
                , ("font-weight", "bold")
                , ("text-align", "center")
                ]
