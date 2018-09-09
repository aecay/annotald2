module Route exposing
    ( Route(..)
    , fromUrl
    , toString
    )

import Set
import Url exposing (Url)
import Parser exposing
    ( Parser
    , oneOf
    , end
    , keyword
    , symbol
    , variable
    , succeed
    , (|=)
    , (|.)
    )


type Route
    = Edit String
    | ListFiles


route : Parser Route
route =
    oneOf
        [ succeed ListFiles |. end
        , succeed ListFiles |. keyword "list-files"
        , Parser.succeed Edit
          |. keyword "edit"
          |. symbol "/"
          |= variable { start = Char.isAlphaNum
                      , inner = \c -> Char.isAlphaNum c || List.member c [ '-', '.', '_' ]
                      , reserved = Set.empty
                      }
        ]

handleFragment : Maybe String -> Route
handleFragment f =
    f |> Maybe.withDefault "" |> Parser.run route |> Result.withDefault ListFiles


fromUrl : Url -> Route
fromUrl u =
    u.fragment
        |> handleFragment

toString : Route -> String
toString r =
    case r of
        Edit file -> "#edit/" ++ file
        ListFiles -> "#list-files"
