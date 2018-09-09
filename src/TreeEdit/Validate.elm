module TreeEdit.Validate exposing (done, perform)

import Array exposing (Array)
import Json.Encode as E
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Return exposing (Return)
import TreeEdit.Dialog exposing (Dialog(..))
import TreeEdit.Metadata.Type as MetadataType
import TreeEdit.Model as Model exposing (Model)
import TreeEdit.Msg as Msg exposing (Msg)
import TreeEdit.OrderedDict as OD
import TreeEdit.Path as Path exposing (Path)
import TreeEdit.Selection as Selection
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Decode exposing (decodeTrees)
import TreeEdit.Tree.Encode exposing (encodeTrees)
import TreeEdit.Tree.Type as TreeType exposing (Tree)
import TreeEdit.Utils as Utils


perform : Model -> Return Msg Model
perform model =
    let
        root =
            .get Model.root model

        sel =
            model.selected |> Selection.getOne |> Maybe.map Path.root

        allTrees : Array Tree
        allTrees =
            root |> OD.values

        trees =
            sel |> Maybe.map (\a -> Tree.get a root) |> Maybe.map (Array.repeat 1) |> Maybe.withDefault allTrees
    in
    Return.return { model | dialog = Just <| Processing "Validating" } <|
        RemoteData.Http.post "/validate"
            (Msg.ValidateDone sel)
            decodeTrees
        <|
            E.object [ ( "trees", encodeTrees trees ) ]


done : Model -> Maybe Path -> WebData (Array Tree) -> Return Msg Model
done model path webdata =
    let
        newModel =
            { model | dialog = Nothing }
    in
    case webdata of
        Success trees ->
            let
                newRoot =
                    case path of
                        Just p ->
                            let
                                root =
                                    .get Model.root model

                                oldTree =
                                    Tree.get p root

                                newTree =
                                    Array.get 0 trees |> Maybe.withDefault oldTree
                            in
                            Tree.set p newTree root

                        Nothing ->
                            Tree.forestFromList <| Array.toList trees

                -- TODO: toList is bogus
            in
            Return.return
                (.set Model.root newRoot newModel)
            <|
                Utils.cmds
                    [ Msg.Metadata MetadataType.NewSelection
                    , Msg.LogMessage "Validate success"
                    ]

        f ->
            Return.return newModel <| Utils.cmd <| Msg.LogMessage <| "Validation failure: " ++ Debug.toString f



-- validatorName : Tree -> R.Result String
-- validatorName tree =
--     let
--         names =
--             tree |>
--             .get Tree.metadata |>
--             Dict.get "VALIDATOR-NAME" |>
--             Maybe.map (String.split "\n") |>
--             Maybe.map (List.filter ((/=) ""))
--     in
--         case names of
--             (Just [name]) -> R.succeed name
--             Nothing -> R.fail "The selected tree passes validation"
--             _ -> R.fail "The selected tree fails multiple validators"
-- id : Tree -> R.Result String
-- id tree =
--     tree |>
--     .metadata |>
--     Dict.get "ID" |>
--     R.liftVal "The selected tree does not have an ID"
-- request : Model -> R.Result (Cmd Msg)
-- request model =
--     let
--         selected = .get Model.selected model |> Selection.getOne |> Utils.fromJust
--         root = .get Model.root model
--         tree_ = Tree.get selected root
--         fileName = .fileName model
--         validatorName_ = R.andThen validatorName tree_
--         treeId_ = Path.root selected |>
--                  flip Tree.get root |>
--                  R.andThen id
--     in
--         R.map (\tree validatorName treeId ->
--                     Http.post "/fix_validator" Msg.FixValidatorDone (D.succeed ()) <|
--                     E.object [ ("filename", E.string fileName)
--                              , ("trees", encodeTrees <| Utils.fromJust <| .getOption Tree.children root)
--                              , ("validator_name", E.string validatorName)
--                              , ("tree_id", E.string treeId)
--                              , ("path", Path.encode selected)
--                              ])
--             tree_ |>
--             R.andMap validatorName_ |>
--             R.andMap treeId_
-- fix : Model -> Return Msg Model
-- fix model =
--     R.succeed model |> R.andDo request |> R.handle model
