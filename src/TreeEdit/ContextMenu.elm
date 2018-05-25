module TreeEdit.ContextMenu exposing ( show
                                     , update
                                     , view
                                     , hide
                                     )

import TreeEdit.View.Utils as ViewUtils exposing (onClick)

import Html as H exposing (Html)
import Html.Attributes as Attr
import Mouse
import Monocle.Lens as Lens
import Return exposing (Return)

import TreeEdit.Config exposing (Config)
import TreeEdit.Path as Path exposing (Path)
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Tree.View exposing (toPenn)
import TreeEdit.Action as Action
import TreeEdit.Msg as Msg
import TreeEdit.Model as Model
import TreeEdit.Model.Type as ModelType
import TreeEdit.Result as R exposing (modify)
import TreeEdit.Utils exposing (fromJust)

import TreeEdit.ContextMenu.Type exposing (..)
import TreeEdit.ContextMenu.Css as CMCss

show : Position -> Path -> ModelType.Model -> ModelType.Model
show position path m =
    { m | contextMenu = Just { position = position, target = path } }

hide : ModelType.Model -> ModelType.Model
hide m = { m | contextMenu = Nothing }

entry : List (H.Attribute Msg) -> String -> Html Msg
entry attrs s = H.div ([ Attr.style CMCss.entry, Attr.class "contextMenuEntry" ] ++ attrs) [ H.a [] [ H.text s ] ]

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

setLabel : Path -> String -> Html Msg
setLabel path label =
    entry [ onClick <| SetLabel path label ] label

heading : String -> Html Msg
heading title = H.div [ Attr.style CMCss.heading ] [ H.text title ]

colWidth : Float
colWidth = 150

column : String -> List (Html Msg) -> Html Msg
column headingText children = H.div [ Attr.style <| CMCss.column ++ [ ("width", toString colWidth ++ "px") ]
                                    ] <|
                          [ heading headingText ] ++ children

view1 : Config -> Model -> String -> Html Msg
view1 config {position, target} label =
    let
        {x, y} = position
        lb = leafBefore target
        la = leafAfter target
        tx = toggleExtension target
        emptyCat {before, node} = ((if before then lb else la) node)
        labels = config.labelGroups |>
                 List.filter (List.any ((==) label)) |>
                 List.head |>
                 Maybe.withDefault ["IP-SUB", "IP-MAT", "IP-INF", "FRAG"] |>
                 List.map (setLabel target)
    in
        H.div [ Attr.style <| CMCss.contextMenu ++
                    [ ("width", (toString <| 3 * colWidth) ++ "px")
                    , ("left", toString x ++ "px")
                    , ("top", toString y ++ "px")
                    ]
              , ViewUtils.onClick Ignore
              ]
            [ column "Label" labels
            , column "Add leaf" <| List.map emptyCat config.insertables
            , column "Toggle ext." <| List.map tx config.dashTags
            ]

view : ModelType.Model -> Html Msg
view parent =
    case parent.contextMenu of
        Nothing -> H.div [] []
        Just model ->
            let
                viewPartial = view1
                label = .get Model.root parent |> Tree.get model.target |> .get Tree.label
            in
                view1 (Model.config parent) model label

update : Msg -> ModelType.Model -> Return Msg.Msg ModelType.Model
update msg model =
    case msg of
        LeafBefore path leaf ->
            .get Model.root model |>
            Action.leafBeforeInner leaf path |>
            R.map (flip (.set Model.root) model) |>
            R.handle model |>
            Return.map hide
        LeafAfter path leaf -> Debug.crash "foo" -- TODO: write path+1
                                                 -- function
        SetLabel path newLabel ->
            modify Model.root
                (Lens.modify (Tree.path path) (.set Tree.label newLabel) >> R.succeed)
                model |>
            R.handle model |>
            Return.map hide
        ToggleExtension path ext -> Action.toggleDashTag ext path model |> R.handle model |> Return.map hide
        Ignore -> Return.singleton model
        Hide -> Return.singleton <| hide model
        Show position path -> Return.singleton <| show position path model

subscriptions : ModelType.Model -> Sub Msg
subscriptions m =
    case m.contextMenu of
        Nothing -> Sub.batch []
        Just _ -> Mouse.clicks (\_ -> Hide)
