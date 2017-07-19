module TreeEdit.Tree.View exposing ( labelString
                                   , terminalString
                                   , toPenn
                                   )

import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Tree, Node(..), TraceType(..), ECType(..))

import TreeEdit.Index as Index

isTrace : Tree -> Bool
isTrace { contents } =
    case contents of
        Trace _ _ -> True
        _ -> False

labelString : Tree -> String
labelString tree =
    let
        index = (.getOption Tree.index) tree |> Maybe.map Index.string |> Maybe.withDefault ""
        label = tree.label
    in
        if isTrace tree
        then label
        else label ++ index

terminalString : Tree -> String
terminalString tree =
    case tree.contents of
        Terminal x _ -> x
        Trace x i ->
            let
                index = toString i
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
        Nonterminal _ _ -> Debug.crash "Can't get the terminalString of a nonterminal"

toPenn : Tree -> String
toPenn = asLabeledBrax1 0

asLabeledBrax1 : Int -> Tree -> String
asLabeledBrax1 indent tree =
    case tree.contents of
        Nonterminal children index -> Debug.crash "not implemented yet"
        Comment s -> "(CODE " ++ s ++ ")" -- TODO: encode comment for cs format
        _ -> "(" ++ labelString tree ++ " " ++ terminalString tree ++ ")"
