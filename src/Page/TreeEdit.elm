module Page.TreeEdit exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html)
import Http exposing (Response(..))
import Json.Decode as D
import Random exposing (Seed)
import Task exposing (Task)
import Url.Builder exposing (absolute, string)

import Return
import TreeEdit.Config as Config
import TreeEdit.Model
import TreeEdit.Model.Type
import TreeEdit.Msg
import TreeEdit.Tree.Decode exposing (decodeTrees)
import TreeEdit.Update
import TreeEdit.View


type alias Msg = TreeEdit.Msg.Msg


type alias Model = TreeEdit.Model.Type.Model


view : Model -> Html Msg
view = TreeEdit.View.view


init : String -> Seed -> ( Model, Cmd Msg )
init filename seed =
    let
        resolver : D.Decoder a -> Http.Response String -> Result Http.Error a
        resolver d r = case r of
                           GoodStatus_ _ s -> D.decodeString d s |> Result.mapError (Http.BadBody << D.errorToString)
                           BadUrl_ url -> Err (Http.BadUrl url)
                           Http.Timeout_ -> Err Http.Timeout
                           Http.NetworkError_ -> Err Http.NetworkError
                           Http.BadStatus_ metadata body -> Err (Http.BadStatus metadata.statusCode)
        getTask : String -> D.Decoder a -> Task Http.Error a
        getTask url decoder = Http.task { method = "GET"
                                        , headers = []
                                        , url = url
                                        , body = Http.emptyBody
                                        , resolver = Http.stringResolver (resolver decoder)
                                        , timeout = Nothing
                                        }
        treesTask =
            getTask (absolute ["file"] [ string "name" filename ]) decodeTrees

        configTask =
            getTask (absolute ["config"] []) Config.decode

        lemmataTask =
            getTask (absolute ["lemmata"] [])
                (D.list D.string)

        jointTask =
            Task.map3 (\a b c -> ( a, b, c )) treesTask configTask lemmataTask
    in
    ( TreeEdit.Model.init filename seed, Task.attempt TreeEdit.Msg.LoadedData jointTask )


update : (String -> Cmd Msg) -> Msg -> Model -> Return.Return Msg Model
update = TreeEdit.Update.update


subscriptions : Model -> Sub Msg
subscriptions = TreeEdit.Update.subscriptions
