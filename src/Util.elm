module Util
    exposing
    ( httpErrorToString
    , webDataToString
    )

import Http
import RemoteData exposing (WebData)

httpErrorToString : Http.Error -> String
httpErrorToString e =
    case e of
        BadUrl s -> "Bad url: " ++ s
        Timeout -> "Timeout"
        NetworkError -> "Network error"
        BadStatus { status, body } -> "HTTP error code " ++
                                      String.fromInt status.code ++
                                      "; body was " ++ body
        BadPayload s { body } ->
            "Payload error: " ++ s ++ "; body was " ++ body

webDataToString : WebData a -> String
webDataToString d =
    case d of
        Success -> "Success"
        NotAsked -> "Request not yet sent"
        Loading -> "Request still in progress"
        Failure f -> "Error: " ++ httpErrorToString f

log : String -> a -> a
log = Debug.log

-- log s x = x
