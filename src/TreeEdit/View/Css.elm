module TreeEdit.View.Css exposing (..)

import TreeEdit.View.Theme exposing (theme)

type alias Style = List (String, String)

-- ip : Style
-- ip = [ ("border-top-color", "black")
--      , ("border-top-width", "1px")
--      , ("border-bottom-color", "black")
--      , ("border-bottom-width", "1px")
--      , ("background-color", theme.salmon)
--      ]

-- selected : Style
-- selected = [ ("background-color", theme.blue) ]

-- snode : Style
-- snode = [ ("margin-left", "20px")
--         , ("border3", "1px solid " ++ theme.silver)
--         , ("border-left-color", theme.blue)
--         , ("border-left-width", "4px")
--         , ("padding", "2px")
--         , ("color", "black")
--         , ("background-color", theme.offWhite)
--         , ("cursor", "pointer")
--         ]

toolbar : Style
toolbar = [ ("top", "30px")
          , ("left", "0px")
          , ("margin-left", "5px")
          , ("width", "15%")
          , ("position", "fixed")
          ]

messages : Style
messages = [ ("bottom", "30px")
           , ("left", "0px")
           , ("margin-left", "5px")
           , ("width", "15%")
           , ("background-color", theme.offWhite2)
           , ("position", "fixed")
           ]

titlebar : Style
titlebar = [ ("background-color", theme.darkGrey)
           , ("color", "white")
           , ("width", "100%")
           , ("height", "16px")
           , ("font-weight", "bold")
           , ("text-align", "center")
           ]

-- sn0 : Style
-- sn0 = [ ("background-color", theme.tan)
--       , ("border", "1px solid black")
--       , ("margin-right", "5%")
--       , ("margin-left", "calc(15% + 12px)")
--       -- In order for the trees to shrink to the correct width
--       , ("display", "inline-block")
--       ]

-- rootSnode : Style
-- rootSnode = [ ("border", "2px solid black") ]

-- wnode : Style
-- wnode = [ ("margin-left", "20px")
--         , ("padding-left", "4px")
--         , ("padding-right", "4px")
--         , ("border", "1px solid black")
--         , ("background-color", "white")
--         , ("color", "black")
--         ]

correctionFlag : Style
correctionFlag = [ ("border", "1px solid #C17900")
                 , ("background-color", "#FCD271")
                 , ("color", "black")
                 , ("fontSize", "6px")
                 , ("padding", "1px")
                 , ("verticalAlign", "text-top")
                 ]
