module TreeEdit.Metadata.Util exposing
    ( EditWidget
    , Formatter
    , capitalize
    , eitherP
    , formatters
    , hasMetadata
    , isAdjective
    , isNominal
    , isPreposition
    , isVerb
    , lemmaSelect
    , lemmaSelectConfig
    , widgets
    , makeLemmata
    )

import Array
import Dict
import Url.Builder exposing (crossOrigin, string)
import Html as Html exposing (Html, li, text, ul)
import Html.Attributes as Attr
import Set exposing (Set)

import Form
import Form.Field as Field
import Form.Input as Input exposing (Input)
import Select

import TreeEdit.Metadata.Css as Css
import TreeEdit.Metadata.Type exposing (Lemma, Model, Msg(..))
import TreeEdit.OrderedDict as OD
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Tree, Forest)
import TreeEdit.View.Theme as Theme


nominalTagInitials : Set Char
nominalTagInitials =
    Set.fromList [ 'N', 'D' ]


hasInitial : Set Char -> Tree -> Bool
hasInitial set t =
    let
        labelInitial =
            .get Tree.label t |> String.uncons |> Maybe.map Tuple.first |> Maybe.withDefault 'x'
    in
    Set.member labelInitial set


hasLabel : String -> Tree -> Bool
hasLabel l t =
    .get Tree.label t == l


isNominal : Tree -> Bool
isNominal =
    eitherP (hasInitial nominalTagInitials) (hasLabel "PPER")


isVerb : Tree -> Bool
isVerb =
    hasInitial <| Set.singleton 'V'


isPreposition : Tree -> Bool
isPreposition =
    hasLabel "APPR"


isAdjective : Tree -> Bool
isAdjective =
    hasInitial <| Set.fromList <| List.singleton 'A'


eitherP : (a -> Bool) -> (a -> Bool) -> a -> Bool
eitherP x y t =
    x t || y t


hasMetadata : String -> Tree -> Bool
hasMetadata key t =
    .get Tree.metadata t |> Dict.get key |> (/=) Nothing


capitalize : String -> String
capitalize s =
    String.toUpper (String.left 1 s) ++ String.dropLeft 1 s


type alias Formatter =
    String -> Html Msg


formatters :
    { definition : Formatter
    , validationError : Formatter
    , value : Formatter
    }
formatters =
    let
        value val =
            if val == "" then
                Html.i Css.textFieldAbsent [ text <| "not present" ]

            else
                text val

        -- Helper functions for definition
        linkify x =
            Html.a [ Attr.href <| crossOrigin "https://www.dict.cc/" [] [string "s" x] ] [ text x ]

        pieces x =
            String.split "," (String.dropLeft 5 x)
                |> List.map String.trim
                |> List.map linkify

        definition val =
            let
                empty =
                    Html.i Css.textFieldAbsent [ text <| "not present" ]

                dictLink =
                    Html.span [] <| List.intersperse (text ", ") (pieces val)
            in
            if val == "" then
                empty

            else if String.startsWith "[de]" val then
                dictLink

            else
                text val

        validationError str =
            String.split "\n" str
                |> List.filter (\x -> x /= "")
                |> List.map (\x -> li [] [ text x ])
                |> ul []
    in
    { value = value
    , definition = definition
    , validationError = validationError
    }


lemmaSelectConfig : Select.Config Msg Lemma
lemmaSelectConfig =
    let
        reset value =
            Form.Reset [ ( "lemma", Field.string value.original ) ]

        onSelect item =
            item |> Maybe.map reset |> Maybe.withDefault Form.NoOp |> Form
    in
    Select.newConfig onSelect .normalized
        |> Select.withCutoff 5
        |> Select.withInputId "lemma-select"
        |> Select.withItemHtml (\i -> Html.li [] [ text i.original ])
        |> Select.withMenuStyles
            [ ( "background-color", .tan Theme.theme )
            , ( "border", "2px solid " ++ .darkTan Theme.theme )
            , ( "width", "100%" )
            ]
        |> Select.withItemStyles
            [ ( "list-style-type", "none" )
            , ( "align", "left" )
            , ( "padding", "4px" )
            ]
        |> Select.withHighlightedItemStyles [ ( "background-color", .salmon Theme.theme ) ]
        |> Select.withClearStyles [ ( "visibility", "hidden" ) ]
        |> Select.withOnQuery LemmaQueryChanged

lemmaFromString : String -> String
lemmaFromString =
    String.replace "oe" "o\u{0308}" >>
        String.replace "ue" "u\u{0308}" >>
        String.replace ":" "\u{0304}"

lemmaSelect : Model -> Form.FieldState () String -> Html Msg
lemmaSelect model state =
    let
        value = state.value |> Maybe.withDefault ""
        values = case model.lemmaInput of
                     Just q ->
                         let
                             lemma = lemmaFromString q
                             lemmaRecord = { original = lemma , normalized = q }
                         in
                              lemmaRecord :: model.lemmata
                     Nothing -> model.lemmata

        initial =
            List.filter (\{ original } -> original == value) values
                |> List.head

    in
    Select.view lemmaSelectConfig model.lemmaSelectState values initial
        |> Html.map LemmaSelect


type alias EditWidget =
    Form.FieldState () String -> Html Msg


widgets :
    { options : List String -> EditWidget
    , textbox : EditWidget
    }
widgets =
    let
        textbox contents =
            Html.map Form <|
                Input.textInput contents [ Attr.style "width" "100%" ]

        options opts contents =
            Html.map Form <|
                Input.selectInput (List.map (\x -> ( x, x )) opts) contents [ Attr.style "width" "100%" ]
    in
    { textbox = textbox
    , options = options
    }

normalizeLemma : String -> String
normalizeLemma =
    String.replace "\u{0304}" "" >> String.replace "\u{0308}" ""

makeLemmata : List String -> Forest -> List Lemma
makeLemmata lemmata forest =
    let
        getLemma tree = .get Tree.metadata tree |> Dict.get "LEMMA" |> Maybe.withDefault "<<none>>"
        update tree set = Set.insert (getLemma tree) set
        forestLemmata = OD.toArray forest
                          |> Array.map Tuple.second
                          |> Array.map (Tree.fold update Set.empty)
                          |> Array.foldl Set.union Set.empty
                          |> Set.remove "<<none>>"
        allLemmata = Set.fromList lemmata |> Set.union forestLemmata
    in
        allLemmata
          |> Set.toList
          |> List.map (\x -> { original = x
                             , normalized = normalizeLemma x
                             })
