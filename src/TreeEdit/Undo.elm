module TreeEdit.Undo exposing (redo, undo)

import Return exposing (Return, return, singleton)
import TreeEdit.Model as Model
import TreeEdit.Model.Type exposing (Model)
import TreeEdit.Msg exposing (Msg(..))
import TreeEdit.Selection as Selection
import TreeEdit.Utils exposing (message)


undo : Model -> Return Msg Model
undo m =
    case m.undo of
        [] ->
            singleton m |> message (LogMessage "No undo information")

        tree :: rest ->
            let
                redoTree =
                    .get Model.root m

                newModel =
                    .set Model.root tree m
            in
            singleton
                { newModel
                    | undo = rest
                    , redo = redoTree :: m.redo
                    , selected = Selection.empty
                }


redo : Model -> Return Msg Model
redo m =
    case m.redo of
        [] ->
            singleton m |> message (LogMessage "No undo information")

        tree :: rest ->
            let
                undoTree =
                    .get Model.root m

                newModel =
                    .set Model.root tree m
            in
            singleton
                { newModel
                    | redo = rest
                    , undo = undoTree :: m.undo
                    , selected = Selection.empty
                }
