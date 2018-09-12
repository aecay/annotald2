module TreeEdit.Config exposing (Config, decode)

import Dict
import Json.Decode as D
import TreeEdit.Tree.Type exposing (Tree, constants)


type alias Insertable =
    { before : Bool
    , node : Tree
    }


tree : D.Decoder Tree
tree =
    D.map3 (\a b c -> ( a, b, c )) (D.field "type" D.string) (D.field "text" D.string) (D.field "label" D.string)
        |> D.andThen
            (\( typ, text, label ) ->
                let
                    info =
                        { label = label
                        , metadata = Dict.empty
                        , index = Nothing
                        }
                in
                case typ of
                    "terminal" ->
                        D.succeed <| constants.ordinary text info

                    "comment" ->
                        D.succeed <| constants.comment text

                    "empty-category" ->
                        let
                            ectype =
                                case text of
                                    "pro" -> D.succeed constants.pro
                                    "con" -> D.succeed constants.con
                                    "zero" -> D.succeed constants.zero
                                    "exp" -> D.succeed constants.exp
                                    _ -> D.fail <| "Unknown ec type" ++ text
                        in
                            ectype |> D.map (\a -> a info)

                    _ ->
                        D.fail <| "Unknown node type " ++ typ
            )


insertable : D.Decoder Insertable
insertable =
    D.map2 Insertable
        (D.oneOf [ D.field "before" D.bool, D.succeed True ])
        tree


type alias Config =
    { ipLabels : List String
    , dashTags : List String
    , insertables : List Insertable
    , labelGroups : List (List String)
    }


decode : D.Decoder Config
decode =
    D.map4 Config
        (D.field "ipLabels" (D.list D.string))
        (D.field "dashTags" (D.list D.string))
        (D.field "insertables" (D.list insertable))
        (D.field "labelGroups" (D.list <| D.list <| D.string))
