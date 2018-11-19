port module TreeEdit.Ports exposing (dirty, editing, openFile, saveScroll, scrollSaved)

import Json.Encode as E

port editing : Bool -> Cmd a

port openFile : String -> Cmd a

port dirty : Bool -> Cmd a

port saveScroll : Bool -> Cmd a

port scrollSaved : (E.Value -> a) -> Sub a
