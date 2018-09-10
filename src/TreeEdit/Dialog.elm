module TreeEdit.Dialog exposing (view)

import Html exposing (Html, button, div, img, text, textarea)
import Html.Attributes as Attr exposing (src, style)
import Html.Events as E
import TreeEdit.Dialog.Type exposing (Dialog(..))
import TreeEdit.Msg exposing (Msg(..))
import TreeEdit.View.Css as ViewCss


copyField : String -> String -> String -> Html Msg
copyField contents id btn =
    div []
        [ textarea [ Attr.id (id ++ "Source"), Attr.readonly True ] [ text contents ]
        , button
            [ Attr.id id
            , E.onClick (SetDialog Nothing)
            , Attr.attribute "data-clipboard-target" ("#" ++ id ++ "Source")
            ]
            [ text btn ]
        ]


view : Dialog -> Html Msg
view dialog =
    let
        title =
            case dialog of
                Copy _ ->
                    "Copy tree"

                Processing _ ->
                    "Processing..."

        view_ =
            case dialog of
                Copy response ->
                    div []
                        [ copyField response.penn "copyButton" "Copy"
                        , copyField response.deep "copyDeepButton" "Copy deep format"
                        , copyField response.text "copyTextButton" "Copy text"
                        ]

                Processing txt ->
                    div
                        [ style "text-align" "center"
                        , style "margin-top" "16px"
                        ]
                        [ text <| "Processing: " ++ txt
                        , div [ style "align" "center" ]
                            [ img [ src "/static/loading.svg" ] [] ]
                        ]
    in
    div
        [ style "z-index" "998"
        , style "opacity" "0.85"
        , style "width" "100%"
        , style "height" "100%"
        , style "position" "fixed"
        , style "left" "0%"
        , style "top" "0%"
        , style "color" "black"
        ]
        [ div
            [ style "position" "fixed"
            , style "top" "25%"
            , style "left" "25%"
            , style "width" "50%"
            , style "height" "50%"
            , style "background-color" "rgb(210,180,140)"
            ]
            [ div ViewCss.titlebar [ text title ]
            , view_
            ]
        ]
