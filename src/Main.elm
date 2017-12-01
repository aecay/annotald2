module Main exposing (..)

import Platform.Cmd as Cmd
import Html
import Html
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Navigation exposing (Location)
import Return

import Route exposing (Route)
import Page.TreeEdit as TreeEdit
import Page.FileList as FileList

type Page =
    FileList FileList.Model |
    TreeEdit TreeEdit.Model

type Msg = TreeEditMsg TreeEdit.Msg |
    FileListMsg FileList.Msg |
    LoadPage (Model, Cmd Msg)

type alias Model = { page : Page }

route : Location -> Msg
route l =
    init l |> LoadPage

init : Location -> (Model, Cmd Msg)
init location =
    case Maybe.withDefault (Route.ListFiles) (Route.fromLocation location) of
        Route.Edit s -> TreeEdit.init s |>
                        Return.mapBoth TreeEditMsg (\x -> { page = TreeEdit x})
        Route.ListFiles -> FileList.init |>
                           Return.mapBoth FileListMsg (\x -> { page = FileList x})

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model.page) of
        (TreeEditMsg submsg, TreeEdit submodel) ->
            TreeEdit.update submsg submodel |>
            Return.mapBoth TreeEditMsg (\x -> { model | page = TreeEdit x})
        (FileListMsg submsg, FileList submodel) ->
             FileList.update submsg submodel |>
             Return.mapBoth FileListMsg (\x -> { model | page = FileList x})
        (LoadPage page, _) ->
            page
        (_, _) ->
            Return.singleton model

view : Model -> Html.Html Msg
view model =
    case model.page of
        FileList submodel -> FileList.view submodel |> Html.map FileListMsg
        TreeEdit submodel -> TreeEdit.view submodel |> Html.map TreeEditMsg

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        FileList submodel -> FileList.subscriptions submodel |> Sub.map FileListMsg
        TreeEdit submodel -> TreeEdit.subscriptions submodel |> Sub.map TreeEditMsg

main : Program Never Model Msg
main =
    Navigation.program route { init = init
                             , update = update
                             , view = view
                             , subscriptions = subscriptions
                             }
