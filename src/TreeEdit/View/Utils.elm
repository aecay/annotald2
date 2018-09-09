module TreeEdit.View.Utils exposing (decodeMouse, onClick)

import Html as H
import Html.Events as Ev
import Json.Decode as Json


import TreeEdit.ContextMenu.Type as ContextMenuType


onClick : a -> H.Attribute a
onClick x =
    Ev.custom "click" (Json.succeed { message = x
                                    , stopPropagation = True
                                    , preventDefault = True
                                    }
                      )


decodeMouse : Json.Decoder ContextMenuType.Position
decodeMouse =
    Json.map2 (\x y -> { x = x, y = y })
        (Json.field "pageX" Json.int)
        (Json.field "pageY" Json.int)
