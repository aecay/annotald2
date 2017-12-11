module TreeEdit.Validate exposing (..)

import Dict
import Json.Decode as D
import Json.Encode as E

import RemoteData.Http as Http

import TreeEdit.Model as Model
import TreeEdit.Model.Type exposing (Model)
import TreeEdit.Msg as Msg exposing (Msg)
import TreeEdit.Path as Path
import TreeEdit.Result as R
import TreeEdit.Selection as Selection
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Tree.Encode exposing (encodeTrees)
import TreeEdit.Utils as Utils

validatorName : Tree -> R.Result String
validatorName tree =
    let
        names =
            tree |>
            .get Tree.metadata |>
            Dict.get "VALIDATOR-NAME" |>
            Maybe.map (String.split "\n")
    in
        case names of
            (Just [name]) -> R.succeed name
            Nothing -> R.fail "The selected tree passes validations"
            _ -> R.fail "The selected tree fails multiple validators"

id : Tree -> R.Result String
id tree =
    tree |>
    .metadata |>
    Dict.get "ID" |>
    R.liftVal "The selected tree does not have an ID"

request : Model -> R.Result (Cmd Msg)
request model =
    let
        selected = .get Model.selected model |> Selection.getOne |> Utils.fromJust
        root = .get Model.root model
        tree_ = Tree.get selected root
        fileName = .fileName model
        validatorName_ = R.map validatorName tree_ |>
                         R.andThen getOnly
        treeId_ = Path.root selected |>
                 flip Tree.get root |>
                 R.map id

    in
        R.map (\tree validatorName treeId ->
                    Http.post "/fix_validator" Msg.FixValidatorDone (D.succeed ()) <|
                    E.object [ ("filename", E.string fileName)
                             , ("trees", encodeTrees <| Utils.fromJust <| .getOption Tree.children root)
                             , ("validator_name", E.string validatorName)
                             , ("tree_id", E.string treeId)
                             , ("path", Path.encode selected)
                             ])
            tree_ |>
            R.andMap validatorName_ |>
            R.andMap treeId_