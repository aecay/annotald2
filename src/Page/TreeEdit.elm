module Page.TreeEdit exposing (init, update, subscriptions, view, Msg, Model)

import Html
import Return
import RemoteData
import RemoteData.Http exposing (getTask, url)
import Task

import TreeEdit.Config as Config
import TreeEdit.Msg as Msg
import TreeEdit.View
import TreeEdit.Update
import TreeEdit.Model
import TreeEdit.Model.Type
import TreeEdit.Tree.Decode exposing (decodeTrees)

type alias Msg = Msg.Msg

type alias Model = TreeEdit.Model.Type.Model

view : TreeEdit.Model.Type.Model -> Html.Html Msg.Msg
view = TreeEdit.View.view

init : String -> ( TreeEdit.Model.Type.Model, Cmd Msg )
init filename =
    let
        treesTask = getTask (url "/file" [("name", filename)]) decodeTrees
        configTask = getTask "/config" Config.decode
        combine = RemoteData.map2 (,)
        jointTask = Task.map2 combine treesTask configTask
    in
        (TreeEdit.Model.init filename, Task.perform Msg.LoadedData jointTask)

update
    : Msg.Msg
    -> TreeEdit.Model.Type.Model
    -> Return.Return Msg.Msg TreeEdit.Model.Type.Model
update = TreeEdit.Update.update

subscriptions : TreeEdit.Model.Type.Model -> Sub Msg.Msg
subscriptions = TreeEdit.Update.subscriptions
