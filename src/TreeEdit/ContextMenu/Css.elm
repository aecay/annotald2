module TreeEdit.ContextMenu.Css exposing (..)

type alias Style = List (String, String)

entry: Style
entry = [ ("color", "#333333")
        , ("text-decoration", "none")
        , ("line-height", "20px")
        , ("height", "20px")
        , ("padding", "1px 5px")
        , ("cursor", "pointer")
        -- TODO , "hover" [ backgroundColor (hex "#cccccc")]
        ]

heading : Style
heading = [ ("color", "#FEEDD5")
          , ("background-color", "black")
          , ("padding", "2px")
          , ("padding-left", "5px")
          , ("border-bottom", "1px solid #c0c0c0")
          , ("border-left", "1px solid #c0c0c0")
          , ("font-weight", "bold")
          ]

column : Style
column = [ ("float", "left") ]


contextMenu : Style
contextMenu = [ ("position", "absolute")
              , ("z-index", "9999")
              , ("border", "1px solid black")
              , ("background-color", "#efefef")
              , ("padding", "0px")
              , ("margin", "0px")
              ]
