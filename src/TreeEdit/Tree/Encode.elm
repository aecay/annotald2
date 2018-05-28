module TreeEdit.Tree.Encode exposing (encodeTrees, encodeTree)

import Array.Hamt as Array exposing (Array)
import Dict
import Json.Encode as E exposing (Value)

import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Tree, TraceType(..), Terminal(..))
import TreeEdit.Tree.View exposing (terminalString)
import TreeEdit.Index as Index exposing (Index)

array_ : (a -> Value) -> Array a -> Value
array_ fn l = E.list <| Array.toList <| Array.map fn l

encodeTrees : Array Tree -> Value
encodeTrees trees = array_ encodeTree trees

indexToJson : Maybe Index -> List (String, String)
indexToJson idx =  case idx of
                       Nothing -> []
                       Just i -> [ ("INDEX", .get Index.number i |> toString)
                                 , ("IDX-TYPE", case .get Index.variety i of
                                                    Index.Normal -> "regular"
                                                    Index.Gap -> "gap")
                                 ]


encodeTree : Tree -> Value
encodeTree tree =
    let
        metadata info = E.object <| List.map (\(k, v) -> (k, E.string v)) <|
                        (Dict.toList info.metadata) ++ (indexToJson info.index)
        nt info children = E.object <| [ ("label", E.string info.label)
                                       , ("children", E.list <| Array.toList <| Array.map encodeTree children)
                                       , ("metadata", metadata info)
                                       ]
        t terminal =
            case terminal of
                Ordinary text info -> E.object <| [ ("label", info.label |> E.string)
                                                  , ("text", text |> E.string)
                                                  , ("metadata", metadata info)
                                                  ]
                Comment string -> E.object [ ("label", "CODE" |> E.string)
                                           , ("text", string |> E.string)
                                           ]
                EmptyCat _ info -> E.object <| [ ("label", info.label |> E.string)
                                               , ("text", terminalString tree |> E.string)
                                               , ("metadata", metadata info)
                                               ]
                Trace traceType info -> E.object <| [ ("label", info.label |> E.string)
                                                    , ("text", E.string <| case traceType of
                                                                               Wh -> "*T*"
                                                                               Extraposition -> "*ICH*"
                                                                               Clitic -> "*CL*")
                                                    , ("metadata", metadata (.get Tree.info tree))
                                                    ]
    in
        Tree.either t nt tree
