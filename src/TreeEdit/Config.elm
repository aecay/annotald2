module TreeEdit.Config exposing (Config, decode)

import Json.Decode as D

type alias Config = { ipLabels : List String
                    , dashTags : List String
                    }

decode : D.Decoder Config
decode = D.map2 Config
         (D.field "ipLabels" (D.list D.string))
         (D.field "dashTags" (D.list D.string))
