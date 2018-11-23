module Util
    exposing
    ( log
    )

log : String -> a -> a
log = Debug.log
-- log s x = x
