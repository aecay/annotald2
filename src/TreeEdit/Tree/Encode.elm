module TreeEdit.Tree.Encode exposing (encodeForest, encodeTree, encodeTrees)

import Array exposing (Array)
import Dict
import Json.Encode as E exposing (Value)
import TreeEdit.Index as Index exposing (Index)
import TreeEdit.OrderedDict as OD
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Forest, Terminal(..), TraceType(..), Tree)
import TreeEdit.Tree.View exposing (terminalString)


encodeForest : Forest -> Value
encodeForest =
    OD.values >> E.array encodeTree


encodeTrees : Array Tree -> Value
encodeTrees trees =
    E.array encodeTree trees


indexToJson : Maybe Index -> List ( String, String )
indexToJson idx =
    case idx of
        Nothing ->
            []

        Just i ->
            [ ( "INDEX", .get Index.number i |> String.fromInt )
            , ( "IDX-TYPE"
              , case .get Index.variety i of
                    Index.Normal ->
                        "regular"

                    Index.Gap ->
                        "gap"
              )
            ]


encodeTree : Tree -> Value
encodeTree tree =
    let
        metadata info =
            E.object <|
                List.map (\( k, v ) -> ( k, E.string v )) <|
                    Dict.toList info.metadata
                        ++ indexToJson info.index

        nt info children =
            E.object <|
                [ ( "label", E.string info.label )
                , ( "children", E.array encodeTree children )
                , ( "metadata", metadata info )
                ]

        t terminal =
            case terminal of
                Ordinary text info ->
                    E.object <|
                        [ ( "label", info.label |> E.string )
                        , ( "text", text |> E.string )
                        , ( "metadata", metadata info )
                        ]

                Comment string ->
                    E.object
                        [ ( "label", "CODE" |> E.string )
                        , ( "text", string |> E.string )
                        ]

                EmptyCat _ info ->
                    E.object <|
                        [ ( "label", info.label |> E.string )
                        , ( "text", terminalString tree |> E.string )
                        , ( "metadata", metadata info )
                        ]

                Trace traceType info ->
                    E.object <|
                        [ ( "label", info.label |> E.string )
                        , ( "text"
                          , E.string <|
                                case traceType of
                                    Wh ->
                                        "*T*"

                                    Extraposition ->
                                        "*ICH*"

                                    Clitic ->
                                        "*CL*"
                          )
                        , ( "metadata", metadata (.get Tree.info tree) )
                        ]
    in
    Tree.either t nt tree
