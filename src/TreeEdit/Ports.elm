port module TreeEdit.Ports exposing (editing, openFile, dirty)

port editing : Bool -> Cmd a

port openFile : String -> Cmd a

port dirty : Bool -> Cmd a
