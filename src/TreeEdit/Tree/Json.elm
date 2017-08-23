module TreeEdit.Tree.Json exposing (toJson)

import Dict
import Json.Encode as E exposing (Value)

import TreeEdit.Tree.Type exposing (Tree, TraceType(..), Node(..))
import TreeEdit.Tree.View exposing (terminalString)
import TreeEdit.Index as Index exposing (Index)

list_ : (a -> Value) -> List a -> Value
list_ fn l = E.list <| List.map fn l

toJson : List Tree -> Value
toJson trees = list_ treeToJson trees

indexToJson : Maybe Index -> List (String, String)
indexToJson idx =  case idx of
                       Nothing -> []
                       Just i -> [ ("INDEX", .get Index.number i |> toString)
                                 , ("IDX-TYPE", case .get Index.variety i of
                                                    Index.Normal -> "normal"
                                                    Index.Gap -> "gap")
                                 ]


treeToJson : Tree -> Value
treeToJson tree =
    let
        metadata idx = E.object <| List.map (\(k, v) -> (k, E.string v)) <| (Dict.toList tree.metadata) ++ (indexToJson idx)
    in
        case tree.contents of
            Nonterminal children idx ->
                let
                    kids = []
                in
                    E.object <| [ ("label", E.string tree.label)
                                , ("children", E.list <| List.map treeToJson children)
                                , ("metadata", metadata idx)
                                ]
            Terminal text index -> E.object <| [ ("label", tree.label |> E.string)
                                               , ("text", text |> E.string)
                                               , ("metadata", metadata index)
                                               ]
            Comment string -> E.object [ ("label", "CODE" |> E.string)
                                       , ("text", string |> E.string)
                                       ]
            EmptyCat _ index -> E.object <| [ ("label", tree.label |> E.string)
                                            , ("text", terminalString tree |> E.string)
                                            , ("metadata", metadata index)
                                            ]
            Trace traceType index -> E.object <| [ ("label", tree.label |> E.string)
                                                 , ("text", E.string <| case traceType of
                                                                            Wh -> "*T*"
                                                                            Extraposition -> "*ICH*"
                                                                            Clitic -> "*CL*")
                                                 , ("metadata", metadata <| Just <| Index.normal index)
                                                 ]
