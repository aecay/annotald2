module Main exposing (..)

import Html
import Platform.Sub as Sub
import Random
import Navigation exposing (Location)

import Return exposing (Return)

import Route exposing (Route)
import Page.TreeEdit as TreeEdit
import Page.FileList as FileList

type Page =
    FileList FileList.Model |
    TreeEdit TreeEdit.Model

type Msg = TreeEditMsg TreeEdit.Msg |
    FileListMsg FileList.Msg |
    LoadPage Route

type alias Model = { page : Page
                   , seed : Random.Seed
                   }

type alias Flags = { randomness : Int
                   }

route : Location -> Msg
route location =
    Maybe.withDefault (Route.ListFiles) (Route.fromLocation location) |> LoadPage

pageInit : Route -> Random.Seed -> Return Msg Page
pageInit route seed =
    case route of
        Route.Edit s -> TreeEdit.init s seed |> Return.mapBoth TreeEditMsg TreeEdit
        Route.ListFiles -> FileList.init |> Return.mapBoth FileListMsg FileList

init : Flags -> Location -> Return Msg Model
init {randomness} location =
    let
        seed = Random.initialSeed randomness
        route = Maybe.withDefault (Route.ListFiles) (Route.fromLocation location)
        (page, cmd) = pageInit route seed
    in
        Return.return
            { page = page
            , seed = seed
            }
            cmd

update : Msg -> Model -> Return Msg Model
update msg model =
    case (msg, model.page) of
        (TreeEditMsg submsg, TreeEdit submodel) ->
            let
                return = TreeEdit.update submsg submodel
            in
                Return.mapBoth TreeEditMsg (\x -> { model | page = TreeEdit x
                                                  , seed = x.seed
                                                  })
                    return
        (FileListMsg submsg, FileList submodel) ->
             FileList.update submsg submodel |>
             Return.mapBoth FileListMsg (\x -> { model | page = FileList x})
        (LoadPage page, _) ->
            let
                (p, cmd) = pageInit page model.seed
            in
                Return.return { model | page = p } cmd
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

main : Program Flags Model Msg
main =
    Navigation.programWithFlags route { init = init
                                      , update = update
                                      , view = view
                                      , subscriptions = subscriptions
                                      }
