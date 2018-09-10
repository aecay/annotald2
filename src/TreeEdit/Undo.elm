module TreeEdit.Undo exposing (redo, undo)

import Return exposing (Return, return, singleton)
import TreeEdit.Model as Model
import TreeEdit.Model.Type exposing (ForestModel)
import TreeEdit.Msg exposing (Msg(..))
import TreeEdit.Selection as Selection
import TreeEdit.Utils exposing (message)


undo : ForestModel -> Return Msg ForestModel
undo m =
    case m.undo of
        [] ->
            singleton m |> message (LogMessage "No undo information")

        tree :: rest ->
            singleton
                { m
                    | root = tree
                    , undo = rest
                    , redo = m.root :: m.redo
                    , selected = Selection.empty
                }


redo : ForestModel -> Return Msg ForestModel
redo m =
    case m.redo of
        [] ->
            singleton m |> message (LogMessage "No undo information")

        tree :: rest ->
            singleton
                { m
                    | root = tree
                    , redo = rest
                    , undo = m.root :: m.undo
                    , selected = Selection.empty
                }
