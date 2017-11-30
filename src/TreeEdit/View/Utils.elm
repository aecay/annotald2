module TreeEdit.View.Utils exposing (onClick, blockAll)

import Html.Styled as H
import Html.Styled.Events as Ev
import Json.Decode as Json

blockAll : Ev.Options
blockAll = { stopPropagation = True
           , preventDefault = True
           }

onClick : a -> H.Attribute a
onClick x = Ev.onWithOptions "click" blockAll (Json.succeed x)
