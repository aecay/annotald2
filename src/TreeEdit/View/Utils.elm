module TreeEdit.View.Utils exposing (onClick, blockAll, decodeMouse)

import Html as H
import Html.Events as Ev
import Json.Decode as Json

import TreeEdit.ContextMenu.Type as ContextMenuType

blockAll : Ev.Options
blockAll = { stopPropagation = True
           , preventDefault = True
           }

onClick : a -> H.Attribute a
onClick x = Ev.onWithOptions "click" blockAll (Json.succeed x)

decodeMouse : Json.Decoder ContextMenuType.Position
decodeMouse = Json.map2 (\x y -> { x = x, y = y })
              (Json.field "pageX" Json.int)
              (Json.field "pageY" Json.int)
