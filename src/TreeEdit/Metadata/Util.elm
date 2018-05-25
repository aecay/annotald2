module TreeEdit.Metadata.Util exposing ( isNominal
                                       , isVerb
                                       , isPreposition
                                       , eitherP
                                       , capitalize
                                       , hasMetadata
                                       , formatters
                                       , Formatter
                                       , widgets
                                       , EditWidget
                                       , lemmaSelect
                                       , lemmaSelectConfig
                                       )

import Dict
import Html as Html exposing (Html, text, li, ul)
import Html.Attributes as Attr
import Http exposing (encodeUri)
import Set exposing (Set)

import Form
import Form.Field as Field
import Form.Input as Input exposing (Input)
import Guards exposing (..)
import Select

import TreeEdit.Metadata.Css as Css
import TreeEdit.Metadata.Type exposing (Msg(Form, LemmaSelect), Model, Lemma)
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.View.Theme as Theme

nominalTagInitials : Set Char
nominalTagInitials = Set.fromList [ 'N', 'D' ]

hasInitial : Set Char -> Tree -> Bool
hasInitial set t =
    let
        labelInitial = .get Tree.label t |> String.uncons |> Maybe.map Tuple.first |> Maybe.withDefault 'x'
    in
        Set.member labelInitial set

hasLabel : String -> Tree -> Bool
hasLabel l t = .get Tree.label t == l

isNominal : Tree -> Bool
isNominal = eitherP (hasInitial nominalTagInitials) (hasLabel "PPER")

isVerb : Tree -> Bool
isVerb = hasInitial <| Set.singleton 'V'

isPreposition : Tree -> Bool
isPreposition = hasLabel "APPR"

eitherP : (a -> Bool) -> (a -> Bool) -> a -> Bool
eitherP x y t = (x t) || (y t)

hasMetadata : String -> Tree -> Bool
hasMetadata key t = .get Tree.metadata t |> Dict.get key |> (/=) Nothing

capitalize : String -> String
capitalize s = String.toUpper (String.left 1 s) ++ String.dropLeft 1 s

type alias Formatter = String -> Html Msg

formatters :
    { definition : Formatter
    , validationError : Formatter
    , value : Formatter
    }
formatters =
    let
        value val =
            if val == ""
            then Html.i [ Attr.style Css.textFieldAbsent ] [ text <| "not present" ]
            else text val

        -- Helper functions for definition
        linkify x = Html.a [Attr.href <| "https://www.dict.cc/?s=" ++ encodeUri x] [text x]
        pieces x = String.split "," (String.dropLeft 5 x) |>
                   List.map String.trim |>
                   List.map linkify
        definition value =
            let
                empty = Html.i [ Attr.style Css.textFieldAbsent ] [ text <| "not present" ]
                dictLink = Html.span [] <| List.intersperse (text ", ") (pieces value)
            in
                   value == ""                    => empty
                |= String.startsWith "[de]" value => dictLink
                |=                                   text value

        validationError str =
            String.split "\n" str |>
            List.filter (\x -> x /= "") |>
            List.map (\x -> li [] [text x]) |>
            ul []
    in
        { value = value
        , definition = definition
        , validationError = validationError
        }

lemmaSelectConfig : Select.Config Msg Lemma
lemmaSelectConfig =
    let
        reset value = Form.Reset [("lemma", Field.string value.original)]
        onSelect item = item |> Maybe.map reset |> Maybe.withDefault Form.NoOp |> Form
    in
        Select.newConfig onSelect .normalized |>
        Select.withCutoff 5 |>
        Select.withInputId "lemma-select" |>
        Select.withItemHtml (\i -> Html.li [] [ text i.original ]) |>
        Select.withMenuStyles [ ("background-color", .tan Theme.theme)
                              , ("border", "2px solid " ++ .darkTan Theme.theme)
                              , ("width", "100%")
                              ] |>
        Select.withItemStyles [ ("list-style-type", "none")
                              , ("align", "left")
                              , ("padding", "4px")
                              ] |>
        Select.withHighlightedItemStyles [ ("background-color", .salmon Theme.theme) ] |>
        Select.withClearStyles [ ("visibility", "hidden") ]

lemmaSelect : Model -> Form.FieldState () String -> Html Msg
lemmaSelect model state =
    let
        value = state.value |> Maybe.withDefault ""
        initial = List.filter (\{original} -> original == value) model.lemmata |>
                  List.head
    in
        Select.view lemmaSelectConfig model.lemmaSelectState model.lemmata initial |>
        Html.map LemmaSelect

type alias EditWidget = Form.FieldState () String -> Html Msg

widgets :
    { options : List String -> EditWidget
    , textbox : EditWidget
    }
widgets =
    let
        textbox contents = Html.map Form <|
                           Input.textInput contents [ Attr.style [("width", "100%")] ]

        options opts contents =
            Html.map Form <|
            Input.selectInput (List.map (\x -> (x,x)) opts) contents [ Attr.style [("width", "100%")] ]
    in
        { textbox = textbox
        , options = options
        }
