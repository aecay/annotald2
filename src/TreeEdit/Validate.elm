module TreeEdit.Validate exposing (perform, done)

import Json.Encode as E

import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Return exposing (Return)

import TreeEdit.Dialog exposing (Dialog(Processing))
import TreeEdit.Metadata.Type as MetadataType
import TreeEdit.Model as Model exposing (Model)
import TreeEdit.Msg as Msg exposing (Msg)
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Tree.Decode exposing (decodeTrees)
import TreeEdit.Tree.Encode exposing (encodeTrees)
import TreeEdit.Utils as Utils


perform : Model -> Return Msg Model
perform model =
    Return.return {model | dialog = Just <| Processing "Validating"} <|
        RemoteData.Http.post "/validate"
            Msg.ValidateDone
            decodeTrees <|
            E.object [ ("trees",
                            .get Model.root model |>
                            .getOption Tree.children |>
                            Utils.fromJust |>
                            encodeTrees
                       )
                     ]

done : Model -> WebData (List Tree) -> Return Msg Model
done model webdata =
    let
        newModel = { model | dialog = Nothing }
    in
        case webdata of
            Success trees -> Return.return
                             (.set Model.root (Tree.t "wtf" trees) newModel)
                             <| Utils.cmds [ Msg.Metadata MetadataType.NewSelection
                                           , Msg.LogMessage "Validate success"
                                           ]
            f -> Return.return newModel <| Utils.cmd <| Msg.LogMessage <| "Validation failure: " ++ toString f

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
