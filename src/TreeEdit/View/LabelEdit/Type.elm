module TreeEdit.View.LabelEdit.Type exposing (Label, LabelForm, Msg(..))

import Form exposing (Form)


type alias Label =
    String


type alias LabelForm =
    Form () Label


type Msg
    = FormMsg Form.Msg
