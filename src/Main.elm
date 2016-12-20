module Main exposing (..)

import Html exposing (program)
import Platform.Cmd as Cmd

import Tree exposing (l, t, Tree)
import Model exposing (Model, withTrees)
import View exposing (view)
import Update exposing (update, subscriptions)
import Msg exposing (Msg)

testTree : Tree
testTree = t "IP-MAT"
           [ t "NP-SBJ"
                [ l "D" "the"
                , l "N" "dog" ]
           , t "VP" [ l "VBD" "barked" , l "ADV" "loudly" ] ]

main : Program Never Model Msg
main = program
       { init = (withTrees [testTree], Cmd.none)
       , view = view
       , update = update
       , subscriptions = subscriptions
       }
