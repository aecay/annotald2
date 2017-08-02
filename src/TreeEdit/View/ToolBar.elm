module TreeEdit.View.ToolBar exposing (view)

import Html exposing (Html, div, text, br, button)
import Html.Attributes as A
import Html.Events exposing (onClick)

import TreeEdit.Msg as Msg exposing (Msg)

view : String -> Html Msg
view filename  = div []
                 [ div [ A.style [ ("background-color", "#2E2E2E")
                                 , ("color", "white")
                                 , ("font-weight", "bold")
                                 , ("text-align", "center")
                                 , ("width", "100%")
                                 , ("height", "16px")
                                ]
                       ] [ text "Annotald 2" ]
                 , div [ A.style [ ("background", "#FEF6EA")
                                 , ("padding-bottom", "4px")
                                 , ("border", "1px solid #857259")
                                 , ("text-align", "center")
                                 , ("margin-bottom", "6px")
                                 ]
                       ]
                     [ text ("Editing " ++ filename)
                     , button [ onClick Msg.DoSave
                              , A.style [("width", "80%")]]
                           [text "Save"]
                     -- Undo, redo, exit
                     ]
                 -- Search, validate
                 ]
