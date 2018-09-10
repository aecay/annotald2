module TreeEdit.View.ToolBar exposing (view)

import Html exposing (Html, br, div, text)
import Html.Attributes as A
import Html.Events as E
import TreeEdit.Msg as Msg exposing (Msg)
import TreeEdit.View.Theme exposing (theme)


box : String -> List (Html Msg) -> Html Msg
box title contents =
    div []
        [ div
            [ A.style "background-color" theme.darkGrey
            , A.style "color" "white"
            , A.style "width" "100%"
            , A.style "height" "16px"
            , A.style "font-weight" "bold"
            , A.style "text-align" "center"
            ]
            [ text title ]
        , div
            [ A.style "background-color" theme.offWhite2
            , A.style "padding-bottom" "4px"
            , A.style "border" ("1px solid " ++ theme.darkTan)
            , A.style "margin-bottom" "6px"
            , A.style "text-align" "center"
            ]
            contents
        ]


button : String -> Msg -> Html Msg
button txt click =
    let
        id = "annotaldButton" ++ txt
    in
        Html.button
            [ E.onClick click
            , E.onFocus (Msg.Blur id)
            , A.style "width" "80%"
            , A.id id
            ]
            [ text txt ]


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
            [ button "Validate" (Msg.Loaded Msg.Validate) ]
        ]
