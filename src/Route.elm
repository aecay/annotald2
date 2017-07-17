module Route exposing (Route(..), goTo, fromLocation)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)
import Navigation exposing (Location)

type Route = Edit String |
    ListFiles

toString : Route -> String
toString r =
    let pieces =
            case r of
                Edit s -> ["edit", s]
                ListFiles -> ["list-files"]
    in
        "#/" ++ String.join "/" pieces

route : Parser (Route -> a) a
route = oneOf
        [ Url.map ListFiles (s "")
        , Url.map ListFiles (s "list-files")
        , Url.map Edit (s "edit" </> string)
        ]

fromLocation : Location -> Maybe Route
fromLocation = parseHash route

goTo : Route -> Cmd a
goTo r = Navigation.modifyUrl <| toString r
