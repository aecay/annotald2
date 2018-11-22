module Main exposing (Flags, Model, Msg(..), Page(..), init, main, pageInit, subscriptions, update, view)

import Browser exposing (UrlRequest(..), Document)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html)
import Platform.Sub as Sub
import Random
import Url exposing (Url)

import Return exposing (Return)

import Page.FileList as FileList
import Page.TreeEdit as TreeEdit
import Route exposing (Route)

import Util exposing (log)

type Page
    = FileList FileList.Model
    | TreeEdit TreeEdit.Model


type Msg
    = TreeEditMsg TreeEdit.Msg
    | FileListMsg FileList.Msg
    | UrlRequested UrlRequest
    | UrlChanged Url


type alias Model =
    { page : Page
    , seed : Random.Seed
    , key : Key
    }


type alias Flags =
    { randomness : Int
    }


pageInit : Route -> Random.Seed -> Return Msg Page
pageInit rte seed =
    case rte of
        Route.Edit s ->
            TreeEdit.init s seed |> Return.mapBoth TreeEditMsg TreeEdit

        Route.ListFiles ->
            FileList.init |> Return.mapBoth FileListMsg FileList


init : Flags -> Url -> Key -> Return Msg Model
init { randomness } url key =
    let
        seed = Random.initialSeed
        rte = Route.fromUrl url
        ( page, cmd ) = pageInit rte seed
    in
    Return.return
        { page = page
        , seed = seed
        , key = key
        }
        cmd


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        TreeEditMsg submsg ->
            case model.page of
                TreeEdit submodel ->
                    let
                        return = TreeEdit.update (Nav.pushUrl model.key) submsg submodel
                    in
                        Return.mapBoth TreeEditMsg
                            (\x ->
                                 { model
                                     | page = TreeEdit x
                                     , seed = x.seed
                                 }
                            )
                            return
                _ ->
                    let
                        _ = log "incongruent message" msg
                    in
                        Return.singleton model

        FileListMsg submsg ->
            case model.page of
                FileList submodel ->
                    FileList.update (Nav.pushUrl model.key) submsg submodel
                        |> Return.mapBoth FileListMsg (\x -> { model | page = FileList x })
                _ ->
                    let
                        _ = log "incongruent message" msg
                    in
                        Return.singleton model

        UrlRequested request ->
            case request of
                Internal url ->
                    Return.return model (Nav.pushUrl model.key <| Url.toString url)
                External _ -> Debug.todo "handle external url request"
        UrlChanged url ->
            let
                _ = log "url" url
                page = Route.fromUrl url
                ( p, cmd ) =
                    pageInit page model.seed
            in
                Return.return { model | page = p } cmd


entitle : Html Msg -> Document Msg
entitle h =
    { title = "Annotald 2.0"
    , body = [ h ]
    }


view : Model -> Document Msg
view model =
    case model.page of
        FileList submodel ->
            FileList.view submodel |> Html.map FileListMsg |> entitle

        TreeEdit submodel ->
            TreeEdit.view submodel |> Html.map TreeEditMsg |> entitle


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        FileList submodel ->
            FileList.subscriptions submodel |> Sub.map FileListMsg

        TreeEdit submodel ->
            TreeEdit.subscriptions submodel |> Sub.map TreeEditMsg


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
