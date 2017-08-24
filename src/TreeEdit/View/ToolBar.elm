module TreeEdit.View.ToolBar exposing (view)

import Color exposing (white)
import Html exposing (Html, div, text, br, button)
import Html.Attributes as A
import Html.Events exposing (onClick)
import TypedStyles exposing ( backgroundColor, color, width, height, px, prc
                            , paddingBottom, solid, border, marginBottom
                            , textCenter
                            )

import TreeEdit.Msg as Msg exposing (Msg)
import TreeEdit.View.Theme exposing (theme)

view : String -> Html Msg
view filename  = div []
                 [ div [ A.style [ backgroundColor theme.darkGrey
                                 , color white
                                 , width 100 prc
                                 , height 16 px
                                 , ("font-weight", "bold")
                                 , textCenter
                                ]
                       ] [ text "Annotald 2" ]
                 , div [ A.style [ backgroundColor theme.offWhite2
                                 , paddingBottom 4 px
                                 , border 1 px solid theme.darkTan
                                 , marginBottom 6 px
                                 , textCenter
                                 ]
                       ]
                     [ text ("Editing " ++ filename)
                     , button [ onClick Msg.DoSave
                              , A.style [ width 80 prc ]
                              ]
                           [text "Save"]
                     -- Undo, redo, exit
                     ]
                 -- Search, validate
                 ]
