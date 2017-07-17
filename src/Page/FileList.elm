module Page.FileList exposing (init, Model, Msg, view, update, subscriptions)

import Html as H
import Html.Events exposing (onClick)

import Json.Decode as D
import RemoteData exposing (WebData, RemoteData(..))
import RemoteData.Http exposing (get)
import Return exposing (return, singleton)

import Route

init : (Model, Cmd Msg)
init = ( { files = NotAsked }, get "/files" GotFiles (D.list D.string) )

type alias Model = { files : WebData (List String) }

type Msg = GotFiles (WebData (List String)) |
    OpenFile String

view : Model -> H.Html Msg
view model =
    let
        -- TODO: underline file names
        openLink fname = H.li [] [H.a [onClick (OpenFile fname)] [H.text fname]]
    in
        case model.files of
            NotAsked -> H.div [] [H.text "Preparing to request file list"]
            Loading -> H.div [] [H.text "Loading file list..."]
            Failure _ -> H.div [] [H.text "Error fetching file list"]
            Success files -> H.ul [] <| List.map openLink files

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotFiles f -> singleton { model | files = f }
        OpenFile file ->
            Route.Edit file |>
            Route.goTo |>
            return model

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
