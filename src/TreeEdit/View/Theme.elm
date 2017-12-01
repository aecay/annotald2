module TreeEdit.View.Theme exposing (theme)

-- import Css exposing (Color, rgb)

type alias Color = String

rgb : Int -> Int -> Int -> String
rgb r g b = "rgb(" ++ toString r ++ "," ++ toString g ++ "," ++ toString b ++ ")"

theme : { offWhite : Color, salmon : Color, silver : Color, blue : Color
        , darkGrey : Color, tan : Color, offWhite2 : Color, darkTan : Color
        }
theme = { blue = rgb 70 130 180
        , salmon = rgb 197 144 142
        , offWhite = rgb 239 239 239
        , silver = rgb 192 192 192
        , tan = rgb 210 180 140
        , darkGrey = rgb 46 46 46
        , offWhite2 = rgb 254 246 234
        , darkTan = rgb 133 114 89
        }
