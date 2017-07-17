module Page.TreeEdit exposing (init, update, subscriptions, view, Msg, Model)

import Html
import Return
import RemoteData.Http exposing (get, url)

import TreeEdit.Msg
import TreeEdit.View
import TreeEdit.Update
import TreeEdit.Model exposing (withTrees)

-- TODO: temporary
import TreeEdit.Tree as Tree exposing (t, l, Tree)

testTrees : List Tree
testTrees = [ t "IP-MAT"
                  [ t "NP-SBJ"
                        [ l "D" "the"
                        , l "N" "dog" ]
                  , t "VP" [ l "VBD" "barked" , l "ADV" "loudly" ]
                  , Tree.trace "NP" 1
                  ]
            , t "IP-MAT" [ Tree.trace "NP" 1 ]
            , t "IP-MAT" [ Tree.trace "NP" 1 ]
            ]

type alias Msg = TreeEdit.Msg.Msg

type alias Model = TreeEdit.Model.Model

view : TreeEdit.Model.Model -> Html.Html TreeEdit.Msg.Msg
view = TreeEdit.View.view

init : String -> ( TreeEdit.Model.Model, Cmd Msg )
init filename = (withTrees testTrees filename,
                get (url "/file" [("name", filename)]) (TreeEdit.Msg.GotTrees) Tree.receiveTrees)

update
    : TreeEdit.Msg.Msg
    -> TreeEdit.Model.Model
    -> Return.Return TreeEdit.Msg.Msg TreeEdit.Model.Model
update = TreeEdit.Update.update

subscriptions : TreeEdit.Model.Model -> Sub TreeEdit.Msg.Msg
subscriptions = TreeEdit.Update.subscriptions
