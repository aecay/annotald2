module Page.FileList exposing (init, Model, Msg, view, update, subscriptions)

import Html as H
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http

import Json.Decode as D
import RemoteData exposing (WebData, RemoteData(..))
import RemoteData.Http exposing (get)
import Return exposing (return, singleton)

import Route

init : (Model, Cmd Msg)
init = ( { files = NotAsked , exited = False }, get "/files" GotFiles (D.list D.string) )

type alias Model = { files : WebData (List String)
                   , exited : Bool
                   }

type Msg = GotFiles (WebData (List String)) |
    OpenFile String |
    Exit |
    Exited

view : Model -> H.Html Msg
view model =
    let
        placeholder = H.span [Attr.style [("flex-grow", "2")]] []
        flexContainer x = H.div [ Attr.style [ ("display", "flex")
                                             , ("justify-content", "spaceBetween")
                                             ]
                                ] x
        openLink fname = H.div [ Attr.style [ ("display", "flex")
                                            , ("justify-content", "spaceBetween")
                                            ]
                               ]
                         [ H.text fname
                         , H.span [Attr.style [("flex-grow", "2")]] []
                         , H.a [ onClick (OpenFile fname)
                               , Attr.style [ ("text-decoration", "underline")
                                            , ("cursor", "pointer")
                                            ]
                               , Attr.title ("Open " ++ fname)
                               ]
                             [ H.text "open" ]
                         ]
        global x = H.div [ Attr.style [ ("width", "600px")
                                      , ("margin-left", "auto")
                                      , ("margin-right", "auto")
                                      , ("margin-top", "16px")
                                      ]
                         ] <|
                   [ H.h1 [ Attr.style [ ("text-align", "center") ] ]
                         [ H.text "Welcome to Annotald version 2.0" ]
                   , H.hr [ Attr.style [ ("border", "none")
                                       , ("padding", "0")
                                       , ("border-top", "medium double #333" )
                                       , ("padding-bottom", "8px")
                                       ]
                          ] []
                   , x
                   , flexContainer [ placeholder
                                   , H.button [ onClick Exit] [ H.text "Exit Annotald" ]
                                   , placeholder
                                   ]
                   ]
    in
        if model.exited
        then H.div [ Attr.style [ ("display", "flex")
                                , ("align-items", "center")
                                , ("justify-content", "center")
                                , ("height", "100vh")
                                , ("font-size", "200%")
                                ]
                   ]
            [ H.text "Annotald has exited, you can close this browser tab now" ]
        else global <|
            case model.files of
                NotAsked -> H.div [] [H.text "Preparing to request file list"]
                Loading -> H.div [] [H.text "Loading file list..."]
                Failure _ -> H.div [] [H.text "Error fetching file list"]
                Success files -> H.div [] <| List.map openLink files

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotFiles f -> singleton { model | files = f }
        OpenFile file ->
            Route.Edit file |>
            Route.goTo |>
            return model
        Exit ->
            let
                request = Http.post "/exit" Http.emptyBody (D.succeed ())
            in
                return model <| Http.send (\_ -> Exited) request
        Exited -> singleton { model | exited = True }


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
