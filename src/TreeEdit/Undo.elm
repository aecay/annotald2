module TreeEdit.Undo exposing (undo, redo)

import Return exposing (Return, singleton)

import TreeEdit.Model as Model
import TreeEdit.Model.Type exposing (Model)
import TreeEdit.Msg exposing (Msg)

undo : Model -> Return Msg Model
undo m =
    let
        undo = m.undo
        h = List.head undo
        r = List.tail undo |> Maybe.withDefault []
    in
        case h of
            Nothing -> singleton { m | lastMessage = "No undo information" }
            Just tree ->
                let
                    redoTree = .get Model.root m
                    newModel = .set Model.root tree m
                in
                    singleton { newModel |
                                undo = r
                              , redo = redoTree :: newModel.redo
                              }

redo : Model -> Return Msg Model
redo m =
    let
        redo = m.redo
        h = List.head redo
        r = List.tail redo |> Maybe.withDefault []
    in
        case h of
            Nothing -> singleton { m | lastMessage = "No redo information" }
            Just tree ->
                let
                    undoTree = .get Model.root m
                    newModel = .set Model.root tree m
                in
                    singleton { newModel |
                                undo = undoTree :: newModel.undo
                              , redo = r
                              }
