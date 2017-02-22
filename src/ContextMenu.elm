module ContextMenu exposing ( Model
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
import Mouse

import Path exposing (Path)
import Tree

import Res exposing (modify)

import Monocle.Lens exposing (Lens)

type alias Position = { x: Int
                      , y: Int
                      }

-- TODO: should really just be one toplevel maybe value
type alias Model = { position : Position
                   , target : Maybe Path
                   }

type Msg a =
    LeafBefore Path String String |
    LeafAfter Path String String |
    SetLabel Path String |
    ToggleExtension Path String |
    Ignore |
    Hide (Lens a Model)

show : Position -> Path -> Lens a Model -> (a -> a)
show position path lens =
    lens.set { position = position, target = Just path }

hide : Lens a Model -> a -> a
hide lens = lens.set emptyModel

emptyModel : Model
emptyModel = { position = { x = 0, y = 0 }, target = Nothing }

entry : List (H.Attribute (Msg a)) -> String -> Html (Msg a)
entry attrs s = H.div [] [ H.a ([ Attr.style [ ("color", "#333")
                                             , ("text-decoration", "none")
                                             , ("line-height", "20px")
                                             , ("height", "20px")
                                             , ("padding", "1px 5px")
                                             , ("padding-left", "28px")
                                             ]
                                ] ++ attrs) [ H.text s ]
                         ]

leaf : String ->
       (Path -> String -> String -> Msg a) ->
       Path -> String -> String -> Html (Msg a)
leaf arrow ctor path label text =
    entry [onClick <| ctor path label text] <|
        arrow ++ " (" ++ label ++ " " ++ text ++ ")"

leafBefore : Path -> String -> String -> Html (Msg a)
leafBefore = leaf "<" LeafBefore

leafAfter : Path -> String -> String -> Html (Msg a)
leafAfter = leaf ">" LeafAfter

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

column : String -> List (Html (Msg a)) -> Html (Msg a)
column headingText children = H.div [ Attr.class "conMenuColumn"
                                    , Attr.style [ ("width", "115px")
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

update : Msg a -> Lens a Tree.Tree -> a -> a
update msg rootLens parent =
    case msg of
        LeafBefore path label text ->
            modify rootLens
                (Tree.l label text |>
                 Tree.insertAt path)
                parent
        LeafAfter path label text -> Debug.crash "foo" -- TODO: write path+1
                                                 -- function
        SetLabel path newLabel ->
            modify rootLens
                (Tree.do path (\x -> {x | label = newLabel }))
                parent
        ToggleExtension path ext -> Debug.crash "foo"
        Ignore -> parent
        -- TODO: need to take model as a parameter, as well as two lenses: to
        -- the root and to the configuration
        Hide lens -> hide lens parent

subscriptions : (Lens a Model) -> a -> Sub (Msg a)
subscriptions l m =
    case .target (l.get m) of
        Nothing -> Sub.batch []
        Just _ -> Mouse.clicks (\_ -> Hide l)
