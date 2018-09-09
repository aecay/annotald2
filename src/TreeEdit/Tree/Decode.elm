module TreeEdit.Tree.Decode exposing (decodeTree, decodeTrees)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder, dict, field, int, lazy, list, string)
import TreeEdit.Index as Index
import TreeEdit.Tree.Type exposing (..)


decodeString : Decoder String
decodeString =
    D.oneOf
        [ string
        , int |> D.map String.fromInt
        ]


type LeafDecoded
    = LeafDecoded String String (Dict String String)


decodeLeaf : Decoder Tree
decodeLeaf =
    D.map3 LeafDecoded
        (field "label" string)
        (field "text" string)
        (field "metadata" (dict decodeString))
        |> D.map mungeLeaf


extractIndex : Dict String String -> ( Dict String String, Maybe Index.Index )
extractIndex metadata =
    let
        index =
            Dict.get "INDEX" metadata

        idxtype =
            Dict.get "IDX-TYPE" metadata

        getInt i =
            i
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 0

        idx =
            case ( index, idxtype ) of
                ( Just n, Just "gap" ) ->
                    Just <| Index.gap <| getInt index

                ( Just n, Just "regular" ) ->
                    Just <| Index.normal <| getInt index

                -- TODO: name mismatch
                ( Nothing, Nothing ) ->
                    Nothing

                _ ->
                    Debug.todo "bad index 2"
    in
    ( metadata |> Dict.remove "INDEX" |> Dict.remove "IDX-TYPE"
    , idx
    )


mungeLeaf : LeafDecoded -> Tree
mungeLeaf (LeafDecoded label text metadata1) =
    private.terminalouter <|
        case label of
            "CODE" ->
                private.comment text

            _ ->
                let
                    ( metadata, i ) =
                        extractIndex metadata1

                    traceindex : Int
                    traceindex =
                        i |> Maybe.map (.get Index.number) |> Maybe.withDefault 1

                    info =
                        { label = label
                        , metadata = metadata
                        , index = i
                        }

                    traceinfo =
                        { label = label
                        , metadata = metadata
                        , index = traceindex
                        }
                in
                case text of
                    "*pro*" ->
                        private.emptycat private.pro info

                    "*con*" ->
                        private.emptycat private.con info

                    "*exp*" ->
                        private.emptycat private.exp info

                    "*" ->
                        private.emptycat private.star info

                    -- TODO: causes problems if we have legitimately the
                    -- text "0" in a document
                    "0" ->
                        private.emptycat private.zero info

                    "*T*" ->
                        private.trace private.wh traceinfo

                    "*ICH*" ->
                        private.trace private.extraposition traceinfo

                    "*CL*" ->
                        private.trace private.clitic traceinfo

                    _ ->
                        private.ordinary text info


type NTDecoded
    = NTDecoded String (Array Tree) (Dict String String)


decodeNonterminal : Decoder Tree
decodeNonterminal =
    D.map3 NTDecoded
        (field "label" string)
        (field "children" (D.map Array.fromList <| list <| lazy <| \_ -> decodeTree))
        (field "metadata" (dict decodeString))
        |> D.map mungeNT


mungeNT : NTDecoded -> Tree
mungeNT (NTDecoded label children metadata1) =
    let
        ( metadata, i ) =
            extractIndex metadata1

        info =
            { label = label
            , metadata = metadata
            , index = i
            }
    in
    private.nonterminal children info


decodeTree : Decoder Tree
decodeTree =
    D.oneOf
        [ decodeNonterminal
        , decodeLeaf
        ]


decodeTrees : Decoder (Array Tree)
decodeTrees =
    D.map Array.fromList <| list decodeTree
