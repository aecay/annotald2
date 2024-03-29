module TreeEdit.ContextMenu exposing
    ( hide
    , show
    , update
    , view
    )

import Browser.Events
import Html as H exposing (Html)
import Html.Attributes as Attr
import Json.Decode as D

import Monocle.Lens as Lens
import RemoteData exposing (RemoteData(..))
import Return exposing (Return)

import TreeEdit.Action as Action
import TreeEdit.Config exposing (Config)
import TreeEdit.ContextMenu.Css as CMCss
import TreeEdit.ContextMenu.Type exposing (..)
import TreeEdit.Model as Model
import TreeEdit.Model.Type as ModelType
import TreeEdit.Msg as Msg
import TreeEdit.Path as Path exposing (Path)
import TreeEdit.Result as R exposing (modify)
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Tree.View exposing (toPenn)
import TreeEdit.View.Utils as ViewUtils exposing (onClick)


show : Position -> Path -> ModelType.ForestModel -> ModelType.ForestModel
show position path m =
    { m | contextMenu = Just { position = position, target = path } }


hide : ModelType.ForestModel -> ModelType.ForestModel
hide m = { m | contextMenu = Nothing }


entry : List (H.Attribute Msg) -> String -> Html Msg
entry attrs s =
    H.div (CMCss.entry ++ [ Attr.class "contextMenuEntry" ] ++ attrs) [ H.span [] [ H.text s ] ]


leaf :
    String
    -> (Path -> Tree -> Msg)
    -> Path
    -> Tree
    -> Html Msg
leaf arrow ctor path newLeaf =
    entry [ onClick <| ctor path newLeaf ] <|
        arrow
            ++ toPenn newLeaf


leafBefore : Path -> Tree -> Html Msg
leafBefore = leaf "< " LeafBefore


leafAfter : Path -> Tree -> Html Msg
leafAfter = leaf "> " LeafAfter


toggleExtension : Path -> String -> Html Msg
toggleExtension path ext =
    entry [ onClick <| ToggleExtension path ext ] ext


setLabel : Path -> String -> Html Msg
setLabel path label =
    entry [ onClick <| SetLabel path label ] label


heading : String -> Html Msg
heading title =
    H.div CMCss.heading [ H.text title ]


colWidth : Float
colWidth =
    150


column : String -> List (Html Msg) -> Html Msg
column headingText children =
    H.div
        (CMCss.column ++ [ Attr.style "width" <| String.fromFloat colWidth ++ "px" ])
        <| (heading headingText) :: children


view1 : Config -> Model -> String -> Html Msg
view1 config { position, target } label =
    let
        { x, y } =
            position

        lb =
            leafBefore target

        la =
            leafAfter target

        tx =
            toggleExtension target

        emptyCat { before, node } =
            (if before then
                lb

             else
                la
            )
                node

        labels =
            config.labelGroups
                |> List.filter (List.any ((==) label))
                |> List.head
                |> Maybe.withDefault [ "IP-SUB", "IP-MAT", "IP-INF", "FRAG" ]
                |> List.map (setLabel target)
    in
    H.div
        (CMCss.contextMenu ++
             [ Attr.style "width" <| (String.fromFloat <| 3 * colWidth) ++ "px"
             , Attr.style "left" <| String.fromInt x ++ "px"
             , Attr.style "top" <| String.fromInt y ++ "px"
             , ViewUtils.onClick Ignore
             ]
        )
        [ column "Label" labels
        , column "Add leaf" <| List.map emptyCat config.insertables
        , column "Toggle ext." <| List.map tx config.dashTags
        ]


view : ModelType.ForestModel -> Html Msg
view parent =
    case parent.contextMenu of
        Nothing ->
            H.div [] []

        Just model ->
            let
                viewPartial =
                    view1

                label =
                    parent.root |> Tree.get model.target |> .get Tree.label
            in
            view1 parent.config model label


update : Msg -> ModelType.ForestModel -> Return Msg.Msg ModelType.ForestModel
update msg model =
    case msg of
        LeafBefore path l ->
            Action.createLeafBefore l model path
              |> R.handle model
              |> Return.map hide

        LeafAfter path l ->
            Action.createLeafAfter l model path
              |> R.handle model
              |> Return.map hide

        SetLabel path newLabel ->
            { model | root = Lens.modify (Tree.path path) (.set Tree.label newLabel) model.root }
              |> R.succeed
              |> R.handle model
              |> Return.map hide

        ToggleExtension path ext ->
            Action.toggleDashTag ext path model
              |> R.handle model
              |> Return.map hide

        Ignore -> Return.singleton model

        Hide -> Return.singleton <| hide model

        Show position path ->
            Return.singleton <| show position path model


subscriptions : ModelType.Model -> Sub Msg
subscriptions m =
    case m.webdata of
        Success fm ->
            case fm.contextMenu of
                Nothing ->
                    Sub.none
                Just _ ->
                    Browser.Events.onClick (D.succeed Hide)
        _ -> Sub.none
