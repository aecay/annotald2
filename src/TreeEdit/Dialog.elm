module TreeEdit.Dialog exposing (Dialog(..), view)

import Html exposing (div, textarea, text, button, Html, img)
import Html.Attributes as Attr exposing (style, src)
import Html.Events as E

import TreeEdit.Clipboard.Type as Clipboard
import TreeEdit.Msg exposing (Msg(DismissDialog))
import TreeEdit.View.Css as ViewCss

type Dialog = Copy Clipboard.Response |
    Processing String

copyField : String -> String -> String -> Html Msg
copyField contents id btn =
    div [] [ textarea [ Attr.id (id ++ "Source"), Attr.readonly True ] [text contents]
           , button [ Attr.id id
                    , E.onClick DismissDialog
                    , Attr.attribute "data-clipboard-target" ("#" ++ id ++ "Source")
                    ]
                 [text btn]
           ]

view : Dialog -> Html Msg
view dialog =
    let
        title = case dialog of
                    Copy _ -> "Copy tree"
                    Processing _ -> "Processing..."
        view = case dialog of
                   Copy response -> div [] [ copyField response.penn "copyButton" "Copy"
                                           , copyField response.deep "copyDeepButton" "Copy deep format"
                                           , copyField response.text "copyTextButton" "Copy text"
                                           ]
                   Processing txt -> div [ style [ ("text-align", "center")
                                                 , ("margin-top", "16px")
                                                 ]
                                         ]
                                     [ text <| "Processing: " ++ txt
                                     , div [ style [ ("align", "center") ] ]
                                         [ img [ src "/static/loading.svg" ] [] ]
                                     ]
    in
        div [ style [ ("z-index", "998")
                    , ("opacity", "0.85")
                    , ("width", "100%")
                    , ("height", "100%")
                    , ("position", "fixed")
                    , ("left", "0%")
                    , ("top", "0%")
                    , ("color", "black")
                    ]
            ] [ div [ style [ ("position", "fixed")
                            , ("top", "25%")
                            , ("left", "25%")
                            , ("width", "50%")
                            , ("height", "50%")
                            , ("background-color", "rgb(210,180,140)")
                            ]
                    ]
                    [ div [ style ViewCss.titlebar ] [text title]
                    , view
                    ]
              ]
