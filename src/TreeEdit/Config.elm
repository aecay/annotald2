module TreeEdit.Config exposing (Config, decode)

import Dict
import Json.Decode as D

import TreeEdit.Tree.Type exposing (Tree, Node(..), ECType(..))

type alias Insertable = { before : Bool
                        , node : Tree
                        }

node : D.Decoder Node
node = D.map2 (,) (D.field "type" D.string) (D.field "text" D.string) |>
       D.andThen (\(typ, text) ->
                 case typ of
                     "terminal" -> D.succeed <| Terminal text Nothing
                     "comment" -> D.succeed <| Comment text
                     "empty-category" ->
                         let
                             ectype = case text of
                                          "pro" -> D.succeed Pro
                                          "con" -> D.succeed Con
                                          "zero" -> D.succeed Zero
                                          "exp" -> D.succeed Exp
                                          _ -> D.fail <| "Unknown ec type" ++ text
                         in
                             ectype |> D.map (\x -> EmptyCat x Nothing)
                     _ -> D.fail <| "Unknown node type " ++ typ
                 )

tree : D.Decoder Tree
tree = D.map3 Tree
       node
       (D.field "label" D.string)
       (D.succeed Dict.empty)

insertable : D.Decoder Insertable
insertable = D.map2 Insertable
             (D.oneOf [D.field "before" D.bool, D.succeed True])
             tree

type alias Config = { ipLabels : List String
                    , dashTags : List String
                    , insertables : List Insertable
                    , labelGroups : List (List String)
                    }

decode : D.Decoder Config
decode = D.map4 Config
         (D.field "ipLabels" (D.list D.string))
         (D.field "dashTags" (D.list D.string))
         (D.field "insertables" (D.list insertable))
         (D.field "labelGroups" (D.list <| D.list <| D.string))
