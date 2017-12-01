module TreeEdit.View.ToolBar exposing (view)

import Html exposing (Html, div, text, br, button)
import Html.Attributes as A
import Html.Events exposing (onClick)

import TreeEdit.Msg as Msg exposing (Msg)
import TreeEdit.View.Theme exposing (theme)

view : String -> Html Msg
view filename  = div []
                 [ div [ A.style [ ("background-color", theme.darkGrey)
                                 , ("color", "white")
                                 , ("width", "100%")
                                 , ("height", "16px")
                                 , ("font-weight", "bold")
                                 , ("text-align", "center")
                                 ]
                       ] [ text "Annotald 2" ]
                 , div [ A.style [ ("background-color", theme.offWhite2)
                                 , ("padding-bottom", "4px")
                                 , ("border", "1px solid " ++ theme.darkTan)
                                 , ("margin-bottom", "6px")
                                 , ("text-align", "center")
                               ]
                       ]
                     [ text ("Editing " ++ filename)
                     , button [ onClick Msg.Save
                              , A.style [ ("width", "80%") ]
                              ]
                           [text "Save"]
                     -- Undo, redo, exit
                     ]
                 -- Search, validate
                 ]
