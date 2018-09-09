module Page.TreeEdit exposing (Model, Msg, init, subscriptions, update, view)

import Html
import Json.Decode as D
import Random exposing (Seed)
import Task
import Url.Builder exposing (absolute, string)

import RemoteData
import RemoteData.Http exposing (getTask)
import Return
import TreeEdit.Config as Config
import TreeEdit.Model
import TreeEdit.Model.Type
import TreeEdit.Msg as Msg
import TreeEdit.Tree.Decode exposing (decodeTrees)
import TreeEdit.Update
import TreeEdit.View


type alias Msg =
    Msg.Msg


type alias Model =
    TreeEdit.Model.Type.Model


view : TreeEdit.Model.Type.Model -> Html.Html Msg.Msg
view =
    TreeEdit.View.view


init : String -> Seed -> ( TreeEdit.Model.Type.Model, Cmd Msg )
init filename seed =
    let
        treesTask =
            getTask (absolute ["file"] [ string "name" filename ]) decodeTrees

        configTask =
            getTask (absolute ["config"] []) Config.decode

        lemmataTask =
            getTask (absolute ["lemmata"] [])
                (D.list
                    (D.map2 (\x y -> { original = x, normalized = y })
                        (D.field "original" D.string)
                        (D.field "normalized" D.string)
                    )
                )

        combine =
            RemoteData.map3 (\a b c -> ( a, b, c ))

        jointTask =
            Task.map3 combine treesTask configTask lemmataTask
    in
    ( TreeEdit.Model.init filename seed, Task.perform Msg.LoadedData jointTask )


update :
    Msg.Msg
    -> TreeEdit.Model.Type.Model
    -> Return.Return Msg.Msg TreeEdit.Model.Type.Model
update =
    TreeEdit.Update.update


subscriptions : TreeEdit.Model.Type.Model -> Sub Msg.Msg
subscriptions =
    TreeEdit.Update.subscriptions
