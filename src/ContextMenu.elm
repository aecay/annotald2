module ContextMenu exposing ( show
                            , update
                            , view
                            , hide
                            , handler
                            )

import ViewUtils exposing (onClick)

import Json.Decode as Json
import Html.Events as Ev

import Html as H exposing (Html)
import Html.Attributes as Attr
import Mouse

import Path exposing (Path)
import Tree exposing (constants, Tree)
import Actions

import Res exposing (modify)

import Monocle.Lens exposing (Lens)

import ContextMenuTypes exposing (..)

show : Position -> Path -> Lens a Model -> (a -> a)
show position path lens =
    lens.set { position = position, target = Just path }

hide : Lens a Model -> a -> a
hide lens = lens.set emptyModel

entry : List (H.Attribute (Msg a)) -> String -> Html (Msg a)
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
       (Path -> Tree -> Msg a) ->
       Path -> Tree -> Html (Msg a)
leaf arrow ctor path newLeaf =
    entry [onClick <| ctor path newLeaf] <|
        arrow ++ Tree.asLabeledBrackets newLeaf

leafBefore : Path -> Tree -> Html (Msg a)
leafBefore = leaf "< " LeafBefore

leafAfter : Path -> Tree -> Html (Msg a)
leafAfter = leaf "> " LeafAfter

toggleExtension : Path -> String -> Html (Msg a)
toggleExtension path ext =
    entry [ onClick <| ToggleExtension path ext ] ext

heading : String -> Html (Msg a)
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

column : String -> List (Html (Msg a)) -> Html (Msg a)
column headingText children = H.div [ Attr.class "conMenuColumn"
                                    , Attr.style [ ("width", toString colWidth ++ "px")
                                                 , ("float", "left")
                                                 ]
                                    ] <|
                          [ heading headingText ] ++ children

view : a -> Lens a Model -> Html (Msg a)
view parent lens =
    let
        model = lens.get parent
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
                                       , ("width", toString (colWidth * 3 + 6) ++ "px")
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

update : Msg a -> Lens a Tree.Tree -> Lens a Model -> a -> a
update msg rootLens configLens parent =
    case msg of
        LeafBefore path leaf ->
            (modify rootLens
                 (Actions.leafBeforeInner leaf path)
                 parent) |> hide configLens
        LeafAfter path leaf -> Debug.crash "foo" -- TODO: write path+1
                                                 -- function
        SetLabel path newLabel ->
            modify rootLens
                (Tree.do path (\x -> {x | label = newLabel }))
                parent
        ToggleExtension path ext -> Debug.crash "foo"
        Ignore -> parent
        Hide -> hide configLens parent
        Show position path -> show position path configLens parent

subscriptions : (Lens a Model) -> a -> Sub (Msg a)
subscriptions l m =
    case .target (l.get m) of
        Nothing -> Sub.batch []
        Just _ -> Mouse.clicks (\_ -> Hide)


blockAll : Ev.Options
blockAll = { stopPropagation = True
           , preventDefault = True
           }

decodeMouse : Json.Decoder ContextMenuTypes.Position
decodeMouse = Json.map2 (\x y -> { x = x, y = y })
              (Json.field "x" Json.int)
              (Json.field "y" Json.int)

handler : Path -> H.Attribute (Msg a)
handler path = Ev.onWithOptions "contextmenu" blockAll <|
          Json.map (\x -> Show x path) decodeMouse
