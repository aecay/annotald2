module TreeEdit.ContextMenu exposing ( show
                                     , update
                                     , view
                                     , hide
                                     )

import TreeEdit.View.Utils as ViewUtils exposing (onClick)

import Css exposing (width, px, left, top)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as Attr
import Mouse
import Return exposing (Return)

import TreeEdit.Path as Path exposing (Path)
import TreeEdit.Tree as Tree exposing (constants)
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Tree.View exposing (toPenn)
import TreeEdit.Actions as Actions
import TreeEdit.Msg as Msg
import TreeEdit.Model as Model
import TreeEdit.Model.Type as ModelType
import TreeEdit.Result as R exposing (modify)

import TreeEdit.ContextMenuTypes exposing (..)
import TreeEdit.ContextMenu.Css as CMCss

show : Position -> Path -> ModelType.Model -> ModelType.Model
show position path =
    .set Model.contextMenu { position = position, target = Just path }

hide : ModelType.Model -> ModelType.Model
hide = .set Model.contextMenu emptyModel

entry : List (H.Attribute Msg) -> String -> Html Msg
entry attrs s = H.div ([ CMCss.entry ] ++ attrs) [ H.a [] [ H.text s ] ]

leaf : String ->
       (Path -> Tree -> Msg) ->
       Path -> Tree -> Html Msg
leaf arrow ctor path newLeaf =
    entry [onClick <| ctor path newLeaf] <|
        arrow ++ toPenn newLeaf

leafBefore : Path -> Tree -> Html Msg
leafBefore = leaf "< " LeafBefore

leafAfter : Path -> Tree -> Html Msg
leafAfter = leaf "> " LeafAfter

toggleExtension : Path -> String -> Html Msg
toggleExtension path ext =
    entry [ onClick <| ToggleExtension path ext ] ext

heading : String -> Html Msg
heading title = H.div [ CMCss.heading ] [ H.text title ]

colWidth : Float
colWidth = 150

column : String -> List (Html Msg) -> Html Msg
column headingText children = H.div [ CMCss.column
                                    , Attr.css [ width (px colWidth) ]
                                    ] <|
                          [ heading headingText ] ++ children

view : ModelType.Model -> Html Msg
view parent =
    let
        model = .get Model.contextMenu parent
        config = Model.config parent
    in
        case model.target of
            Nothing -> H.div [] []
            Just path ->
                let
                    lb = leafBefore path
                    la = leafAfter path
                    tx = toggleExtension path
                in
                    H.div [ CMCss.contextMenu
                          , Attr.css [ width (px <| 3 * colWidth)
                                     , left (px <| toFloat model.position.x)
                                     , top (px <| toFloat model.position.y)
                                     ]
                          , ViewUtils.onClick Ignore
                          ]
                        [ column "Label" []
                        , column "Add leaf" [ lb constants.con
                                            , lb constants.pro
                                            , lb constants.czero
                                            , lb constants.comment
                                            , la constants.comment
                                            ]
                        , column "Toggle ext." <| List.map tx config.dashTags
                        ]

update : Msg -> ModelType.Model -> Return Msg.Msg ModelType.Model
update msg model =
    case msg of
        LeafBefore path leaf ->
            .get Model.root model |>
            Actions.leafBeforeInner leaf path |>
            R.map (flip (.set Model.root) model) |>
            R.handle model |>
            Return.map hide
        LeafAfter path leaf -> Debug.crash "foo" -- TODO: write path+1
                                                 -- function
        SetLabel path newLabel ->
            modify Model.root
                (Tree.do path (\x -> {x | label = newLabel }))
                model |>
            R.handle model
        ToggleExtension path ext -> Actions.toggleDashTag ext path model |> R.handle model |> Return.map hide
        Ignore -> Return.singleton model
        Hide -> Return.singleton <| hide model
        Show position path -> Return.singleton <| show position path model

subscriptions : ModelType.Model -> Sub Msg
subscriptions m =
    case .target (.get Model.contextMenu m) of
        Nothing -> Sub.batch []
        Just _ -> Mouse.clicks (\_ -> Hide)
