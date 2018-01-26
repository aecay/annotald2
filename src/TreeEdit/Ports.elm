port module TreeEdit.Ports exposing (editing, openFile)

port editing : Bool -> Cmd a

port openFile : String -> Cmd a
