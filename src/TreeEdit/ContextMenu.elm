module TreeEdit.ContextMenu exposing ( show
                                     , update
                                     , view
                                     , hide
                                     )

import TreeEdit.View.Utils as ViewUtils exposing (onClick)

import Html as H exposing (Html)
import Html.Attributes as Attr
import Mouse
import Return exposing (Return)

import TreeEdit.Config exposing (Config)
import TreeEdit.Path as Path exposing (Path)
import TreeEdit.Tree as Tree exposing (constants)
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Tree.View exposing (toPenn)
import TreeEdit.Action as Action
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

heading : String -> Html Msg
heading title = H.div [ Attr.style CMCss.heading ] [ H.text title ]

colWidth : Float
colWidth = 150

column : String -> List (Html Msg) -> Html Msg
column headingText children = H.div [ Attr.style <| CMCss.column ++ [ ("width", toString colWidth ++ "px") ]
                                    ] <|
                          [ heading headingText ] ++ children

view1 : Config -> {x: Int, y: Int} -> Path -> Html Msg
view1 config {x, y} path =
    let
        lb = leafBefore path
        la = leafAfter path
        tx = toggleExtension path
        emptyCat {before, node} = ((if before then lb else la) node)
    in
        H.div [ Attr.style <| CMCss.contextMenu ++
                    [ ("width", (toString <| 3 * colWidth) ++ "px")
                    , ("left", toString x ++ "px")
                    , ("top", toString y ++ "px")
                    ]
              , ViewUtils.onClick Ignore
              ]
            [ column "Label" []
            , column "Add leaf" <| List.map emptyCat config.insertables
            , column "Toggle ext." <| List.map tx config.dashTags
            ]

view : ModelType.Model -> Html Msg
view parent =
    let
        model = .get Model.contextMenu parent
        viewPartial = view1 (Model.config parent) model.position
    in
        Maybe.map viewPartial model.target |>
        Maybe.withDefault (H.div [] [])

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
                (Tree.do path (\x -> {x | label = newLabel }) >> R.liftVal "contextMenu update")
                model |>
            R.handle model
        ToggleExtension path ext -> Action.toggleDashTag ext path model |> R.handle model |> Return.map hide
        Ignore -> Return.singleton model
        Hide -> Return.singleton <| hide model
        Show position path -> Return.singleton <| show position path model

subscriptions : ModelType.Model -> Sub Msg
subscriptions m =
    case .target (.get Model.contextMenu m) of
        Nothing -> Sub.batch []
        Just _ -> Mouse.clicks (\_ -> Hide)
