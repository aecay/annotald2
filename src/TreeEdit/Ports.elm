port module TreeEdit.Ports exposing (editing, openFile, dirty, saveScroll)

port editing : Bool -> Cmd a

port openFile : String -> Cmd a

port dirty : Bool -> Cmd a

port saveScroll : () -> Cmd a
