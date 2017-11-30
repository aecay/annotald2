module TreeEdit.View.ToolBar exposing (view)

import Css.Colors exposing (white)
import Html.Styled exposing (Html, div, text, br, button)
import Html.Styled.Attributes as A
import Html.Styled.Events exposing (onClick)
import Css exposing ( backgroundColor, color, width, height, px, pct
                    , paddingBottom, solid, border3, marginBottom
                    , textAlign, center, fontWeight, bold
                    )

import TreeEdit.Msg as Msg exposing (Msg)
import TreeEdit.View.Theme exposing (theme)

view : String -> Html Msg
view filename  = div []
                 [ div [ A.css [ backgroundColor theme.darkGrey
                               , color white
                               , width (pct 100)
                               , height (px 16)
                               , fontWeight bold
                               , textAlign center
                               ]
                       ] [ text "Annotald 2" ]
                 , div [ A.css [ backgroundColor theme.offWhite2
                               , paddingBottom (px 4)
                               , border3 (px 1) solid theme.darkTan
                               , marginBottom (px 6)
                               , textAlign center
                               ]
                       ]
                     [ text ("Editing " ++ filename)
                     , button [ onClick Msg.Save
                              , A.css [ width (pct 80) ]
                              ]
                           [text "Save"]
                     -- Undo, redo, exit
                     ]
                 -- Search, validate
                 ]
