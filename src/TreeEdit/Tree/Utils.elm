module TreeEdit.Tree.Utils exposing (hasTerminalLabel)

import TreeEdit.Tree.Type exposing (..)

hasTerminalLabel : Tree -> Bool
hasTerminalLabel {contents} =
    case contents of
        Terminal _ _ -> True
        Trace _ _ -> False
        Comment _ -> False
        EmptyCat Pro _ -> False
        EmptyCat Con _ -> False
        EmptyCat Exp _ -> False
        EmptyCat Star _ -> True
        EmptyCat Zero _ -> True
        Nonterminal _ _ -> False
