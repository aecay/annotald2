module TreeEdit.Tree.Type exposing (..)

import Dict exposing (Dict)

import TreeEdit.Index as Index

type TraceType = Wh | Extraposition | Clitic

type ECType = Pro | Con | Exp | Star | Zero

type Node = Terminal String (Maybe Index.Index) |
    Trace TraceType Int |
    Comment String |
    EmptyCat ECType (Maybe Index.Index) |
    Nonterminal (List Tree) (Maybe Index.Index)

type alias Label = String

type alias Tree = { contents: Node
                  , label: Label
                  , metadata: Dict String String
                  }
