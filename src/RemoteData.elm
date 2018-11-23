module RemoteData
    exposing ( WebData
             , RemoteData(..)
             , fromResult
             , toString
             , httpErrorToString
             )
import Http exposing (Error(..))

type RemoteData e a =
    NotAsked
  | Loading
  | Failure e
  | Success a

type alias WebData a = RemoteData Http.Error a

fromResult : Result Http.Error a -> WebData a
fromResult r =
    case r of
        Ok a -> Success a
        Err e -> Failure e

httpErrorToString : Http.Error -> String
httpErrorToString e =
    case e of
        BadUrl s -> "Bad url: " ++ s
        Timeout -> "Timeout"
        NetworkError -> "Network error"
        BadStatus status -> "HTTP error code " ++ String.fromInt status
        BadBody s -> "Payload error: " ++ s

toString : WebData a -> String
toString d =
    case d of
        Success _ -> "Success"
        NotAsked -> "Request not yet sent"
        Loading -> "Request still in progress"
        Failure f -> "Error: " ++ httpErrorToString f
