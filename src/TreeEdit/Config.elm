module TreeEdit.Config exposing (Config, decode)

import Json.Decode as D

type alias Config = { ipLabels : List String
                    }

decode : D.Decoder Config
decode = D.map Config
         (D.field "ipLabels" (D.list D.string))
