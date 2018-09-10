module TreeEdit.Dialog.Type exposing (Dialog(..))

import TreeEdit.Clipboard.Type as Clipboard

type Dialog
    = Copy Clipboard.Response
    | Processing String
