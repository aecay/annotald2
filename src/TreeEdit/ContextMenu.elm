module TreeEdit.ContextMenu exposing ( show
                                     , update
                                     , view
                                     , hide
                                     )

import TreeEdit.ViewUtils as ViewUtils exposing (onClick)

import Html as H exposing (Html)
import Html.Attributes as Attr
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

show : Position -> Path -> ModelType.Model -> ModelType.Model
show position path =
    .set Model.contextMenu { position = position, target = Just path }

hide : ModelType.Model -> ModelType.Model
hide = .set Model.contextMenu emptyModel

entry : List (H.Attribute Msg) -> String -> Html Msg
entry attrs s = H.div [] [ H.a ([ Attr.style [ ("color", "#333")
                                             , ("text-decoration", "none")
                                             , ("line-height", "20px")
                                             , ("height", "20px")
                                             , ("padding", "1px 5px")
                                             -- , ("padding-left", "28px")
                                             , ("cursor", "pointer")
                                             ]
                                ] ++ attrs) [ H.text s ]
                         ]

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
heading title = H.div [ Attr.style [ ("color", "#FEEDD5")
                                   , ("background-color", "black")
                                   , ("padding", "2px")
                                   , ("padding-left", "5px")
                                   , ("border-bottom", "1px solid silver")
                                   , ("border-left", "1px solid silver")
                                   , ("font-weight", "bold")
                                   ]
                      ] [ H.text title ]

colWidth : Int
colWidth = 150

column : String -> List (Html Msg) -> Html Msg
column headingText children = H.div [ Attr.class "conMenuColumn"
                                    , Attr.style [ ("width", toString colWidth ++ "px")
                                                 , ("float", "left")
                                                 ]
                                    ] <|
                          [ heading headingText ] ++ children

view : ModelType.Model -> Html Msg
view parent =
    let
        model = .get Model.contextMenu parent
    in
        case model.target of
            Nothing -> H.div [] []
            Just path ->
                let
                    lb = leafBefore path
                    la = leafAfter path
                    tx = toggleExtension path
                in
                    H.div [ Attr.id "conMenu"
                          , Attr.style [ ("position", "absolute")
                                       , ("width", toString (colWidth * 3) ++ "px")
                                       , ("z-index", "9999")
                                       , ("border" , "1px solid black")
                                       , ("background-color", "#efefef")
                                       , ("padding", "0px")
                                       , ("margin", "0px")
                                       , ("left", toString model.position.x ++ "px")
                                       , ("top", toString model.position.y ++ "px")
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
                        , column "Toggle ext." [ tx "SPE"
                                               , tx "XXX"
                                               ] -- TODO: real list of extensions to toggle
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
        ToggleExtension path ext -> Debug.crash "foo"
        Ignore -> Return.singleton model
        Hide -> Return.singleton <| hide model
        Show position path -> Return.singleton <| show position path model

subscriptions : ModelType.Model -> Sub Msg
subscriptions m =
    case .target (.get Model.contextMenu m) of
        Nothing -> Sub.batch []
        Just _ -> Mouse.clicks (\_ -> Hide)
