module TreeEdit.Undo exposing (undo, redo)

import Return exposing (Return, singleton, return)

import TreeEdit.Model as Model
import TreeEdit.Model.Type exposing (Model)
import TreeEdit.Msg exposing (Msg(LogMessage))
import TreeEdit.Selection as Selection
import TreeEdit.Utils exposing (message)

undo : Model -> Return Msg Model
undo ({undo, redo} as m) =
    case undo of
        [] -> singleton m |> message (LogMessage "No undo information")
        tree :: rest ->
            let
                redoTree = .get Model.root m
                newModel = .set Model.root tree m
            in
                singleton { newModel |
                            undo = rest
                          , redo = redoTree :: redo
                          , selected = Selection.empty
                          }


redo : Model -> Return Msg Model
redo ({undo, redo} as m) =
    case redo of
        [] -> singleton m |> message (LogMessage "No undo information")
        tree :: rest ->
            let
                undoTree = .get Model.root m
                newModel = .set Model.root tree m
            in
                singleton { newModel |
                            redo = rest
                          , undo = undoTree :: undo
                          , selected = Selection.empty
                          }