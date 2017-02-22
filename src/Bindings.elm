module Bindings exposing (bindings)

import Keyboard

import Actions

import Dict exposing (Dict)

bindings : Dict Keyboard.KeyCode Actions.Action
bindings = Dict.fromList [ ( 32, Actions.clearSelection )
                         , ( 119, Actions.changeLabel
                                ["NP-SBJ", "NP-OB1", "NP-OB2", "NP-PRD"])
                         , ( 99, Actions.coIndex)
                         , ( 120, Actions.createParent "XP" )
                         , ( 100, Actions.deleteNode )
                         ]
