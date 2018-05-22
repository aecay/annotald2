module TreeEdit.View.ToolBar exposing (view)

import Html exposing (Html, div, text, br)
import Html.Attributes as A
import Html.Events as E

import TreeEdit.Msg as Msg exposing (Msg)
import TreeEdit.View.Theme exposing (theme)

box : String -> List (Html Msg) -> Html Msg
box title contents =
    div []
        [ div [ A.style [ ("background-color", theme.darkGrey)
                        , ("color", "white")
                        , ("width", "100%")
                        , ("height", "16px")
                        , ("font-weight", "bold")
                        , ("text-align", "center")
                        ]
              ] [ text title ]
        , div [ A.style [ ("background-color", theme.offWhite2)
                        , ("padding-bottom", "4px")
                        , ("border", "1px solid " ++ theme.darkTan)
                        , ("margin-bottom", "6px")
                        , ("text-align", "center")
                        ]
              ] contents
        ]

button : String -> Msg -> Html Msg
button txt click = Html.button [ E.onClick click,  A.style [ ("width", "80%") ] ] [ text txt ]

view : String -> Html Msg
view filename =
    div []
        [ box "Annotald 2"
              [ text ("Editing " ++ filename)
              , button "Save" Msg.Save
              , button "Exit" Msg.Exit
              -- Undo, redo, exit
              ]
        , box "Tools"
            [ button "Validate" Msg.Validate ]
        ]
