module TreeEdit.Bindings exposing (bindings)

import Char exposing (toCode)
import Dict exposing (Dict)
import Keyboard

import TreeEdit.Actions as Actions

import TreeEdit.Tree exposing (constants)

bindings : Dict Keyboard.KeyCode Actions.Action
bindings = Dict.fromList <|
           List.map (\(char, binding) -> (toCode char, binding)) <|
           [ ( '2' , Actions.changeLabel ["NP", "NP-POS"] )
           , ( ' ', Actions.clearSelection )
           , ( 'q', Actions.changeLabel ["CONJP"] )
           , ( 'w', Actions.changeLabel ["NP-SBJ", "NP-OB1", "NP-OB2", "NP-PRD"] )
           , ( 'e', Actions.changeLabel ["CP-ADV", "CP-CMP", "CP-DEG"])
           , ( 'r', Actions.changeLabel ["CP-REL", "CP-FRL", "CP-CAR", "CP-CLF"])
           , ( 't', Actions.changeLabel ["CP-THT", "CP-QUE"])
           , ( 'a', Actions.leafAfter constants.vb )
           , ( 's', Actions.changeLabel ["IP-SUB", "IP-MAT", "IP-IMP"])
           , ( 'd', Actions.deleteNode )
           , ( 'f', Actions.changeLabel ["PP", "ADVP", "ADVP-TMP", "ADVP-LOC", "ADVP-DIR"])
           , ( 'g', Actions.changeLabel ["ADJP", "ADJP-SPR", "NP-MSR", "QP"])
           -- , ( 'z', ) -- TODO: undo
           , ( 'x', Actions.createParent "XP" )
           , ( 'c', Actions.coIndex )
           , ( 'v', Actions.changeLabel ["IP-SMC", "IP-INF"])
           , ( 'b' , Actions.leafBefore constants.con )
           , ( 'l' , Actions.editLabel )
           ]
