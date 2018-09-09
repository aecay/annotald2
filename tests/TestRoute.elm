module TestRoute exposing (..)

import Test exposing (..)
import Expect

import Url exposing (Url, Protocol(..))
import Url.Builder exposing (custom, Root(..))

import Route exposing (fromUrl, toString, Route(..))

hash : String -> Url
hash s = { protocol = Https
         , host = ""
         , port_ = Nothing
         , path = ""
         , query = Nothing
         , fragment = Just s
         }

suite : Test
suite = describe "Route" <|
        [ describe "toString" <|
              [ test "edit" <| \() -> Expect.equal "#edit/foo" <| toString <| Edit "foo"
              , test "list files" <| \() -> Expect.equal "#list-files" <| toString <| ListFiles
              ]
        , describe "fromUrl" <|
            [ test "empty url" <| \() -> Expect.equal ListFiles <| fromUrl <| hash ""
            , test "list files" <| \() -> Expect.equal ListFiles <| fromUrl <| hash "list-files"
            , test "edit" <| \() -> Expect.equal (Edit "foo.psd") <| fromUrl <| hash "edit/foo.psd"
            ]
        -- , describe "handleFragment" <|
        --     [ test "empty url" <| \() -> Expect.equal ListFiles <| handleFragment <| Just ""
        --     , test "empty url" <| \() -> Expect.equal ListFiles <| handleFragment <| Nothing
        --     , test "list files" <| \() -> Expect.equal ListFiles <| handleFragment <| Just "list-files"
        --     , test "edit" <| \() -> Expect.equal (Edit "foo.psd") <| handleFragment <| Just "edit/foo.psd"
        --     ]
        ]
