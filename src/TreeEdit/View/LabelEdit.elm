module TreeEdit.View.LabelEdit exposing (view, update, init, finish)

import Form exposing (FieldState)
import Form.Field as Field
import Form.Input as Input
import Form.Validate as V exposing (Validation)
import Html as Html exposing (Html)
import Html.Attributes as Attr
import Return exposing (Return)

import TreeEdit.Tree as Tree
import TreeEdit.Result as R exposing (Result)
import TreeEdit.View.LabelEdit.Type exposing (..)

type alias Model = LabelForm

validation : Validation () Label
validation = V.field "label" V.string

view : LabelForm -> Html Msg
view form = Html.map FormMsg <| Input.textInput
            (Form.getFieldAsString "label" form)
            [ Attr.style [("width", "150px")]
            , Attr.id "labelEditor"
            ]

init : String -> LabelForm
init label = Form.initial [("label", Field.string label)] validation

update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        FormMsg submsg -> model |>
                          Form.update validation submsg |>
                          Return.singleton

finish : LabelForm -> R.Result String
finish form =
    case Form.getOutput form of
        Just newlabel ->
            let
                l = String.toUpper newlabel
            in
                if String.any Tree.illegalLabelChar l
                then R.fail "Illegal characters in label"
                else R.succeed l
        _ -> R.fail "no label form"
