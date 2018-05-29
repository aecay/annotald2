module TreeEdit.Bindings exposing (bindings)

import Char exposing (toCode)
import Dict exposing (Dict)
import Keyboard

import TreeEdit.Action as Action exposing (actions)
import TreeEdit.Clipboard as Clipboard
import TreeEdit.Tree.Type exposing (constants)

bindings : Dict (Int, Keyboard.KeyCode) Action.Action
bindings = Dict.fromList <|
           [ ( (0, toCode '2'), actions.changeLabel ["NP", "NP-POS", "NP-VOC"] )
           , ( (0, toCode ' '), actions.clearSelection )
           , ( (0, toCode 'Q'), actions.changeLabel ["CONJP"] )
           , ( (0, toCode 'W'), actions.changeLabel ["NP-SBJ", "NP-OB1", "NP-OB2", "NP-PRD"] )
           , ( (0, toCode 'E'), actions.changeLabel ["CP-ADV", "CP-CMP", "CP-DEG"])
           , ( (0, toCode 'R'), actions.changeLabel ["CP-REL", "CP-FRL", "CP-CAR", "CP-CLF"])
           , ( (0, toCode 'T'), actions.changeLabel ["CP-THT", "CP-QUE"])
           , ( (0, toCode 'A'), actions.leafAfter constants.vb )
           , ( (0, toCode 'S'), actions.changeLabel ["IP-SUB", "IP-MAT", "IP-IMP"])
           , ( (0, toCode 'D'), actions.deleteNode )
           , ( (0, toCode 'F'), actions.changeLabel ["PP", "ADVP", "ADVP-TMP", "ADVP-LOC", "ADVP-DIR"])
           , ( (0, toCode 'G'), actions.changeLabel ["ADJP", "ADJP-SPR", "NP-MSR", "QP"])
           , ( (0, toCode 'X'), actions.createParent "XP" )
           , ( (0, toCode 'C'), actions.coIndex )
           , ( (0, toCode 'V'), actions.changeLabel ["IP-INF", "RRC", "IP-PPL", "IP-SMC"])
           , ( (0, toCode 'B'), actions.leafBefore constants.con )
           , ( (0, toCode 'L'), actions.editLabel )
           , ( (1, toCode 'C'), Clipboard.copy )
           , ( (0, toCode 'Z'), actions.undo )
           , ( (1, toCode 'Z'), actions.redo )
           ]
