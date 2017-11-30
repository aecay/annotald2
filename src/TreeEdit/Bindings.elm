module TreeEdit.Bindings exposing (bindings)

import Char exposing (toCode)
import Dict exposing (Dict)
import Keyboard

import TreeEdit.Actions as Actions
import TreeEdit.Clipboard as Clipboard
import TreeEdit.Tree exposing (constants)

bindings : Dict (Int, Keyboard.KeyCode) Actions.Action
bindings = Dict.fromList <|
           [ ( (0, toCode '2'), Actions.changeLabel ["NP", "NP-POS", "NP-VOC"] )
           , ( (0, toCode ' '), Actions.clearSelection )
           , ( (0, toCode 'Q'), Actions.changeLabel ["CONJP"] )
           , ( (0, toCode 'W'), Actions.changeLabel ["NP-SBJ", "NP-OB1", "NP-OB2", "NP-PRD"] )
           , ( (0, toCode 'E'), Actions.changeLabel ["CP-ADV", "CP-CMP", "CP-DEG"])
           , ( (0, toCode 'R'), Actions.changeLabel ["CP-REL", "CP-FRL", "CP-CAR", "CP-CLF"])
           , ( (0, toCode 'T'), Actions.changeLabel ["CP-THT", "CP-QUE"])
           , ( (0, toCode 'A'), Actions.leafAfter constants.vb )
           , ( (0, toCode 'S'), Actions.changeLabel ["IP-SUB", "IP-MAT", "IP-IMP"])
           , ( (0, toCode 'D'), Actions.deleteNode )
           , ( (0, toCode 'F'), Actions.changeLabel ["PP", "ADVP", "ADVP-TMP", "ADVP-LOC", "ADVP-DIR"])
           , ( (0, toCode 'G'), Actions.changeLabel ["ADJP", "ADJP-SPR", "NP-MSR", "QP"])
           , ( (0, toCode 'X'), Actions.createParent "XP" )
           , ( (0, toCode 'C'), Actions.coIndex )
           , ( (0, toCode 'V'), Actions.changeLabel ["IP-INF", "RRC", "IP-PPL", "IP-SMC"])
           , ( (0, toCode 'B'), Actions.leafBefore constants.con )
           , ( (0, toCode 'L'), Actions.editLabel )
           , ( (1, toCode 'C'), Clipboard.copy )
           ]
