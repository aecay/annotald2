module TreeEdit.Dialog exposing (Dialog(..), view)

import Html exposing (div, textarea, text, button, Html, img)
import Html.Attributes as Attr exposing (style, src)
import Html.Events as E

import TreeEdit.Msg exposing (Msg(DismissDialog))
import TreeEdit.View.Css as ViewCss

type Dialog = Copy String |
    Processing String

view : Dialog -> Html Msg
view dialog =
    let
        title = case dialog of
                    Copy _ -> "Copy tree"
                    Processing _ -> "Processing..."
        view = case dialog of
                   Copy txt -> div [] [ textarea [ Attr.id "copySource", Attr.readonly True ] [text txt]
                                      , button [ Attr.id "copyButton"
                                               , E.onClick DismissDialog
                                               , Attr.attribute "data-clipboard-target" "#copySource"
                                               ]
                                            [text "Copy"]
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
