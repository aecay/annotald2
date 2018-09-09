port module TreeEdit.Ports exposing (dirty, editing, openFile, saveScroll)


port editing : Bool -> Cmd a


port openFile : String -> Cmd a


port dirty : Bool -> Cmd a


port saveScroll : () -> Cmd a
