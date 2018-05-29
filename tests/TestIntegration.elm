module TestIntegration exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)

import Array.Hamt as Array
import Dict
import List.Extra
import Maybe.Extra
import Monocle.Lens as Lens
import RemoteData exposing (RemoteData(Success))
import UuidStream

import TreeEdit.Config exposing (Config)
import TreeEdit.Model as Model
import TreeEdit.Model.Type exposing (Model)
import TreeEdit.Msg as Msg exposing (Msg)
import TreeEdit.Path as Path
import TreeEdit.Tree as Tree
import TreeEdit.Tree.View as TreeView
import TreeEdit.Tree.Type as TreeType exposing (Tree)
import TreeEdit.Update as Update

p = .fromList Path.internals

t = .t TreeType.private
l s = .l TreeType.private s "x"

dummyConfig : Config
dummyConfig = { ipLabels = []
              , dashTags = []
              , insertables = []
              , labelGroups = []
              }

withTrees : List Tree -> Model
withTrees trees =
    Model.init "foo" |>
    (\x -> Update.update (Msg.LoadedData (Success (Array.fromList trees,
                                                       dummyConfig,
                                                       [])))
         x
         (UuidStream.uuidStringStream 1 [1,2,3,4])) |>
    Tuple.first |>
    Tuple.first

perform : List Tree -> List Msg -> List Tree
perform trees_ msgs =
    let
        addId tree (uuids, trees) =
            let
                (newId, newUuids) = UuidStream.consume uuids
                newTree = Lens.modify Tree.metadata (Dict.insert "ID" newId) tree
            in
                (newUuids, newTree :: trees)
        (uuids, trees) = List.foldr addId (UuidStream.uuidStringStream 1 [1,2,3,4], []) trees_
        model = withTrees trees
        update msg (model, uuids) =
            let
                ((model_, _), uuids_) = Update.update msg model uuids
            in
                (model_, uuids_) -- TODO: ignores cmds....
    in
        List.foldl update (model, uuids) msgs |>
        Tuple.first |>
        .get Model.root |>
        .get Tree.children |>
        Array.toList

suite : Test
suite = describe "Integration tests" <|
        [ describe "Adding IDs in appropriate circumstances" <|
          [ test "moving to the root node" <|
            \() -> perform [ t "FOO"
                                 [ t "BAR" [l "bar"]
                                 , t "QUUX" [l "quux"]
                                 ]
                           ]
                [ Msg.ToggleSelect (p [1,0])
                , Msg.RightClickRoot
                ] |>
                List.Extra.getAt 1 |>
                Maybe.andThen (.get Tree.metadata >> Dict.get "ID") |>
                Maybe.Extra.isJust |>
                Expect.true "should have had an ID"
          ]
        ]
