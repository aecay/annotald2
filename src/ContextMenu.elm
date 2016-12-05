module ContextMenu exposing (Model
                            , emptyModel
                            , Position
                            , Msg
                            , show
                            , update
                            , view
                            , hide)

-- import Model exposing (Model)
import ViewUtils exposing (onClick)

import Html as H exposing (Html)
import Html.Attributes as Attr

import Tree exposing (Path)

import Utils exposing ((?>), (?>?))

import TreeExts as TX

import Monocle.Lens exposing (Lens)

type alias Position = { x: Int
                      , y: Int
                      }

-- TODO: should really just be one toplevel maybe value
type alias Model = { position : Position
                   , target : Maybe Path
                   }

type Msg =
    LeafBefore Path String String |
    LeafAfter Path String String |
    SetLabel Path String |
    ToggleExtension Path String |
    Ignore

show : Position -> Path -> Lens a Model -> (a -> a)
show position path lens =
    lens.set { position = position, target = Just path }

hide : Lens a Model -> a -> a
hide lens = lens.set emptyModel

emptyModel : Model
emptyModel = { position = { x = 0, y = 0 }, target = Nothing }

entry : List (H.Attribute Msg) -> String -> Html Msg
entry attrs s = H.div [] [ H.a ([ Attr.style [ ("color", "#333")
                                             , ("text-decoration", "none")
                                             , ("line-height", "20px")
                                             , ("height", "20px")
                                             , ("padding", "1px 5px")
                                             , ("padding-left", "28px")
                                             ]
                                ] ++ attrs) [ H.text s ]
                         ]

leaf : String -> (Path -> String -> String -> Msg) -> Path -> String -> String -> Html Msg
leaf arrow ctor path label text =
    entry [onClick <| ctor path label text] <|
        arrow ++ " (" ++ label ++ " " ++ text ++ ")"

leafBefore : Path -> String -> String -> Html Msg
leafBefore = leaf "<" LeafBefore

leafAfter : Path -> String -> String -> Html Msg
leafAfter = leaf ">" LeafAfter

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

column : String -> List (Html Msg) -> Html Msg
column headingText children = H.div [ Attr.class "conMenuColumn"
                                    , Attr.style [ ("width", "115px")
                                                 , ("float", "left")
                                                 ]
                                    ] <|
                          [ heading headingText ] ++ children

view : Model -> Html Msg
view model =
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
                                   , ("width", "345px")
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
                    , column "Add leaf" [ lb "NP-SBJ" "*con*"
                                        , lb "NP-SBJ" "*pro*"
                                        , lb "C" "0"
                                        , lb "CODE" "{COM:XXX}"
                                        , la "CODE" "{COM:XXX}"
                             ]
                    , column "Toggle ext." [ tx "SPE"
                                           , tx "XXX"
                                           ] -- TODO: real list of extensions to toggle
                    ]

update : Msg -> Tree.Tree -> Maybe Tree.Tree
update msg =
    case msg of
        LeafBefore path label text ->
            Tree.l label text |>
            Tree.insertAt path
        LeafAfter path label text -> \x -> Debug.crash "foo" -- TODO: write path+1
                                                 -- function
        SetLabel path newLabel ->
            -- TODO: use doAt from Actions.elm
            \m ->
                Tree.get path m ?>
                TX.updateDatum (\x -> {x | label = newLabel }) ?>?
                \x -> Tree.set path x m
        ToggleExtension path ext -> \x -> Debug.crash "foo"
        Ignore -> \_ -> Nothing

-- TODO: hide context menu when appropriate START HERE
