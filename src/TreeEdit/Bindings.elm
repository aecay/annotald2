module TreeEdit.Bindings exposing (bindings)

import Char exposing (toCode)
import Dict exposing (Dict)
import Keyboard

import TreeEdit.Action as Action
import TreeEdit.Clipboard as Clipboard
import TreeEdit.Tree exposing (constants)

bindings : Dict (Int, Keyboard.KeyCode) Action.Action
bindings = Dict.fromList <|
           [ ( (0, toCode '2'), Action.changeLabel ["NP", "NP-POS", "NP-VOC"] )
           , ( (0, toCode ' '), Action.clearSelection )
           , ( (0, toCode 'Q'), Action.changeLabel ["CONJP"] )
           , ( (0, toCode 'W'), Action.changeLabel ["NP-SBJ", "NP-OB1", "NP-OB2", "NP-PRD"] )
           , ( (0, toCode 'E'), Action.changeLabel ["CP-ADV", "CP-CMP", "CP-DEG"])
           , ( (0, toCode 'R'), Action.changeLabel ["CP-REL", "CP-FRL", "CP-CAR", "CP-CLF"])
           , ( (0, toCode 'T'), Action.changeLabel ["CP-THT", "CP-QUE"])
           , ( (0, toCode 'A'), Action.leafAfter constants.vb )
           , ( (0, toCode 'S'), Action.changeLabel ["IP-SUB", "IP-MAT", "IP-IMP"])
           , ( (0, toCode 'D'), Action.deleteNode )
           , ( (0, toCode 'F'), Action.changeLabel ["PP", "ADVP", "ADVP-TMP", "ADVP-LOC", "ADVP-DIR"])
           , ( (0, toCode 'G'), Action.changeLabel ["ADJP", "ADJP-SPR", "NP-MSR", "QP"])
           , ( (0, toCode 'X'), Action.createParent "XP" )
           , ( (0, toCode 'C'), Action.coIndex )
           , ( (0, toCode 'V'), Action.changeLabel ["IP-INF", "RRC", "IP-PPL", "IP-SMC"])
           , ( (0, toCode 'B'), Action.leafBefore constants.con )
           , ( (0, toCode 'L'), Action.editLabel )
           , ( (1, toCode 'C'), Clipboard.copy )
           ]
