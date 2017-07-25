module TreeEdit.Tree.Json exposing (toJson)

import Json.Encode as E exposing (Value)

import TreeEdit.Tree.Type exposing (Tree, TraceType(..), Node(..))
import TreeEdit.Tree.View exposing (terminalString)
import TreeEdit.Index as Index exposing (Index)

list_ : (a -> Value) -> List a -> Value
list_ fn l = E.list <| List.map fn l

toJson : List Tree -> Value
toJson trees = list_ treeToJson trees

indexToJson : Maybe Index -> List (String, Value)
indexToJson idx =  case idx of
                       Nothing -> []
                       Just i -> [("metadata", E.object
                                       [ ("INDEX", .get Index.number i |> toString |> E.string)
                                       , ("IDX-TYPE", E.string <| case .get Index.variety i of
                                                                      Index.Normal -> "normal"
                                                                      Index.Gap -> "gap")]
                                  )]

treeToJson : Tree -> Value
treeToJson tree =
    case tree.contents of
        Nonterminal children idx ->
            let
                kids = [("children", E.list <| List.map treeToJson children)]
            in
                E.object <| [ ("label", E.string tree.label) ] ++ kids ++ indexToJson idx
        Terminal text index -> E.object <| [ ("label", tree.label |> E.string)
                                           , ("text", text |> E.string)
                                           ] ++ (indexToJson index)
        Comment string -> E.object [ ("label", "CODE" |> E.string)
                                   , ("text", string |> E.string)
                                   ]
        EmptyCat _ index -> E.object <| [ ("label", tree.label |> E.string)
                                        , ("text", terminalString tree |> E.string)
                                        ] ++ (indexToJson index)
        Trace traceType index -> E.object <| [ ("label", tree.label |> E.string)
                                             , ("text", E.string <| case traceType of
                                                                        Wh -> "*T*"
                                                                        Extraposition -> "*ICH*"
                                                                        Clitic -> "*CL*")
                                             ] ++ (indexToJson <| Just <| Index.normal index)
