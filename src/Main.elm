module Main exposing (..)

import Platform.Cmd as Cmd
import Html
import Html
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Navigation exposing (Location)

import List.Extra
import Return exposing (Return)
import UuidStream exposing (UuidStream)

import Route exposing (Route)
import Page.TreeEdit as TreeEdit
import Page.FileList as FileList

import TreeEdit.Utils exposing (fromJust) -- FIXME: encapsulation violation

type Page =
    FileList FileList.Model |
    TreeEdit TreeEdit.Model

type Msg = TreeEditMsg TreeEdit.Msg |
    FileListMsg FileList.Msg |
    LoadPage (Page, Cmd Msg)

type alias Model = { page : Page
                   , uuids : UuidStream String
                   }

type alias Flags = { randomness : List Int
                   }

route : Location -> Msg
route l =
    pageInit l |> LoadPage

pageInit : Location -> Return Msg Page
pageInit location =
    case Maybe.withDefault (Route.ListFiles) (Route.fromLocation location) of
        Route.Edit s -> TreeEdit.init s |> Return.mapBoth TreeEditMsg TreeEdit
        Route.ListFiles -> FileList.init |> Return.mapBoth FileListMsg FileList

init : Flags -> Location -> Return Msg Model
init {randomness} location =
    let
        randomness_ = List.Extra.uncons randomness |> fromJust
        (page, cmd) = pageInit location
    in
        Return.return
            { page = page
            , uuids = uncurry UuidStream.uuidStringStream randomness_
            }
            cmd

update : Msg -> Model -> Return Msg Model
update msg model =
    case (msg, model.page) of
        (TreeEditMsg submsg, TreeEdit submodel) ->
            let
                (return, uuids) = TreeEdit.update submsg submodel model.uuids
            in
                Return.mapBoth TreeEditMsg (\x -> { model | page = TreeEdit x
                                                  , uuids = uuids
                                                  })
                    return
        (FileListMsg submsg, FileList submodel) ->
             FileList.update submsg submodel |>
             Return.mapBoth FileListMsg (\x -> { model | page = FileList x})
        (LoadPage (page, cmd), _) ->
            Return.return { model | page = page } cmd
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

main : Program (List Int) Model Msg
main =
    Navigation.programWithFlags route { init = init
                                      , update = update
                                      , view = view
                                      , subscriptions = subscriptions
                                      }
