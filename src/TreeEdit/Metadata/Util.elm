module TreeEdit.Metadata.Util exposing ( isNominal
                                       , isVerb
                                       , capitalize
                                       , hasMetadata
                                       )

import Dict
import Set exposing (Set)

import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Tree)

nominalTagInitials : Set Char
nominalTagInitials = Set.fromList [ 'N', 'D' ]

hasInitial : Set Char -> Tree -> Bool
hasInitial set t =
    let
        labelInitial = .label t |> String.uncons |> Maybe.map Tuple.first |> Maybe.withDefault 'x'
    in
        Set.member labelInitial set

isNominal : Tree -> Bool
isNominal = hasInitial nominalTagInitials

isVerb : Tree -> Bool
isVerb = hasInitial <| Set.singleton 'V'

hasMetadata : String -> Tree -> Bool
hasMetadata key t = .get Tree.metadata t |> Dict.get key |> (/=) Nothing

capitalize : String -> String
capitalize s = String.toUpper (String.left 1 s) ++ String.dropLeft 1 s
