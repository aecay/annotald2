module Main exposing (..)

import Html exposing (program)
import Platform.Cmd as Cmd

import Tree exposing (l, t, Tree)
import Model exposing (Model, withTrees)
import View exposing (view)
import Update exposing (update, subscriptions)
import Msg exposing (Msg)

testTrees : List Tree
testTrees = [ t "IP-MAT"
                  [ t "NP-SBJ"
                        [ l "D" "the"
                        , l "N" "dog" ]
                  , t "VP" [ l "VBD" "barked" , l "ADV" "loudly" ]
                  , Tree.trace "NP" 1
                  ]
            , t "IP-MAT" [ Tree.trace "NP" 1 ]
            , t "IP-MAT" [ Tree.trace "NP" 1 ]
            ]

main : Program Never Model Msg
main = program
       { init = (withTrees testTrees, Cmd.none)
       , view = view
       , update = update
       , subscriptions = subscriptions
       }
