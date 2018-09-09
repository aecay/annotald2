module TreeEdit.Bindings exposing (get)

import Char exposing (toCode)
import Dict exposing (Dict)
import ThirdParty.KeyboardKey as K
import TreeEdit.Action as Action exposing (actions)
import TreeEdit.Clipboard as Clipboard
import TreeEdit.Tree.Type exposing (constants)


get : (Int, K.Key) -> Maybe Action.Action
get binding =
    bindings
        |> List.filter (\(k, b) -> k == binding)
        |> List.head
        |> Maybe.map Tuple.second

bindings : List ( ( Int, K.Key ), Action.Action )
bindings =
        [ ( ( 0, K.Two      ), actions.changeLabel [ "NP", "NP-POS", "NP-VOC" ] )
        , ( ( 0, K.Spacebar ), actions.clearSelection )
        , ( ( 0, K.Q        ), actions.changeLabel [ "CONJP" ] )
        , ( ( 0, K.W        ), actions.changeLabel [ "NP-SBJ", "NP-OB1", "NP-OB2", "NP-PRD" ] )
        , ( ( 0, K.E        ), actions.changeLabel [ "CP-ADV", "CP-CMP", "CP-DEG" ] )
        , ( ( 0, K.R        ), actions.changeLabel [ "CP-REL", "CP-FRL", "CP-CAR", "CP-CLF" ] )
        , ( ( 0, K.T        ), actions.changeLabel [ "CP-THT", "CP-QUE" ] )
        , ( ( 0, K.A        ), actions.leafAfter constants.vb )
        , ( ( 0, K.S        ), actions.changeLabel [ "IP-SUB", "IP-MAT", "IP-IMP" ] )
        , ( ( 0, K.D        ), actions.deleteNode )
        , ( ( 0, K.F        ), actions.changeLabel [ "PP", "ADVP", "ADVP-TMP", "ADVP-LOC", "ADVP-DIR" ] )
        , ( ( 0, K.G        ), actions.changeLabel [ "ADJP-PRD", "ADJP-SPR", "ADJP-SMC" ] )
        , ( ( 0, K.X        ), actions.createParent "XP" )
        , ( ( 0, K.C        ), actions.coIndex )
        , ( ( 0, K.V        ), actions.changeLabel [ "IP-INF", "RRC", "IP-PPL", "IP-SMC" ] )
        , ( ( 0, K.B        ), actions.leafBefore constants.con )
        , ( ( 0, K.L        ), actions.editLabel )
        , ( ( 1, K.C        ), Clipboard.copy )
        , ( ( 0, K.Z        ), actions.undo )
        , ( ( 1, K.Z        ), actions.redo )
        ]
