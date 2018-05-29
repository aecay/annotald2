module TreeEdit.Tree.View exposing ( labelString
                                   , terminalString
                                   , toPenn
                                   )

import Array.Hamt as Array
import Dict

import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Tree, Terminal(..), TraceType(..), ECType(..))

import TreeEdit.Index as Index

isTrace : Tree -> Bool
isTrace tree =
    let
        nt _ _ = False
        t terminal =
            case terminal of
                Trace _ _ -> True
                _ -> False
    in
        Tree.either t nt tree

labelString : Tree -> String
labelString tree =
    let
        index = (.get Tree.index) tree |> Maybe.map Index.string |> Maybe.withDefault ""
        label = .get Tree.label tree
    in
        if isTrace tree
        then label
        else label ++ index

terminalString : Tree -> String
terminalString tree =
    let
        nt _ _ = Debug.crash "Can't get the terminalString of a nonterminal"
        t terminal =
            case terminal of
                Ordinary x _ -> x
                Trace x info ->
                    let
                        index = info |> .index |> toString
                        trace = case x of
                                    Wh -> "*T*"
                                    Extraposition -> "*ICH*"
                                    Clitic -> "*CL*"
                    in
                        trace ++ "-" ++ index
                Comment c -> "{COM:" ++ c ++ "}"
                EmptyCat x _ -> case x of
                                    Pro -> "*pro*"
                                    Con -> "*con*"
                                    Exp -> "*exp*"
                                    Star -> "*"
                                    Zero -> "0"
    in
        Tree.either t nt tree

toPenn : Tree -> String
toPenn = asLabeledBrax1 0

asLabeledBrax1 : Int -> Tree -> String
asLabeledBrax1 indent tree =
    let
        nt info kids =
            let
                label = .label info
                labelLength = String.length label
                childString = Array.toList kids |>
                              List.map (asLabeledBrax1 0) |>
                              List.intersperse "\n" |>
                              String.concat
                childString_ = case Dict.get "ID" <| .metadata info of
                                   Just id -> childString ++ "\n(ID " ++ id ++ ")"
                                   Nothing -> childString
            in
                "(" ++ label ++ " " ++ childString_ ++ ")"
        t terminal =
            case terminal of
                Comment s -> "(CODE " ++ s ++ ")" -- TODO: encode comment for cs format
                _ -> "(" ++ labelString tree ++ " " ++ terminalString tree ++ ")"
    in
        Tree.either t nt tree
