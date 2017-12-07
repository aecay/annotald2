module TreeEdit.Tree.Decode exposing (decodeTrees, decodeTree)

import TreeEdit.Tree.Type exposing (..)

import TreeEdit.Index as Index

import Json.Decode as D exposing (list, string, Decoder, dict, field, lazy, int)
import Dict exposing (Dict)

decodeString : Decoder String
decodeString =
    D.oneOf [ string
            , int |> D.map toString
            ]

type LeafDecoded = LeafDecoded String String (Dict String String)

decodeLeaf : Decoder Tree
decodeLeaf =
    D.map3 LeafDecoded
        (field "label" string)
        (field "text" string)
        (field "metadata" (dict decodeString)) |>
        D.map mungeLeaf

extractIndex : Dict String String -> (Dict String String, Maybe Index.Index)
extractIndex metadata =
    let
        index = Dict.get "INDEX" metadata
        idxtype = Dict.get "IDX-TYPE" metadata
        getInt i = i |>
                   Maybe.withDefault "0" |>
                   String.toInt |>
                   Result.withDefault 0
        i = case (index, idxtype) of
                (Just n, Just "gap") -> Just <| Index.gap <| getInt index
                (Just n, Just "regular") -> Just <| Index.normal <| getInt index -- TODO: name mismatch
                (Nothing, Nothing) -> Nothing
                _ -> Debug.crash "bad index 2"
    in
        ( metadata |> Dict.remove "INDEX" |> Dict.remove "IDX-TYPE"
        , i
        )

mungeLeaf : LeafDecoded -> Tree
mungeLeaf l =
    case l of LeafDecoded label text metadata1 -> -- TODO: destructuring is
                                                 -- ugly, use a type alias/
                                                 -- object instead
        case label of
            "CODE" -> { label = "CODE", contents = Comment text, metadata = metadata1 }
            _ ->
                let
                    (metadata, i) = extractIndex metadata1
                    trace typ = i |>
                                Maybe.withDefault (Index.normal 0) |>
                                .get Index.number |>
                                Trace typ
                in
                    case text of
                        "*pro*" -> { label = label, contents = EmptyCat Pro i, metadata = metadata }
                        "*con*" -> { label = label, contents = EmptyCat Con i, metadata = metadata }
                        "*exp*" -> { label = label, contents = EmptyCat Exp i, metadata = metadata }
                        "*" ->     { label = label, contents = EmptyCat Star i, metadata = metadata }
                        -- TODO: causes problems if we have legitimately the
                        -- text "0" in a document
                        "0" ->     { label = label, contents = EmptyCat Zero i, metadata = metadata }
                        "*T*" ->   { label = label , contents = trace Wh, metadata = metadata }
                        "*ICH*" -> { label = label, contents = trace Extraposition, metadata = metadata }
                        "*CL*" ->  { label = label, contents = trace Clitic, metadata = metadata }
                        _ -> { label = label, contents = Terminal text i, metadata = metadata }

type NTDecoded = NTDecoded String (List Tree) (Dict String String)

decodeNonterminal : Decoder Tree
decodeNonterminal = D.map3 NTDecoded
                    (field "label" string )
                    (field "children" (list <| lazy <| \_ -> decodeTree))
                    (field "metadata" (dict decodeString)) |>
                    D.map mungeNT

mungeNT : NTDecoded -> Tree
mungeNT n =
    case n of NTDecoded label children metadata1 ->
        let
            (metadata, i) = extractIndex metadata1
        in
            { label = label, contents = Nonterminal children i, metadata = metadata }

decodeTree : Decoder Tree
decodeTree =
    D.oneOf [ decodeNonterminal
            , decodeLeaf
            ]

decodeTrees : Decoder (List Tree)
decodeTrees = (list decodeTree)
