module TreeEdit.View exposing (view)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Lazy exposing (lazy3)
import Json.Decode as Json
import Html.Events as Ev

import TreeEdit.Model as Model exposing (Model)
import TreeEdit.Tree as Tree exposing (Tree)
import TreeEdit.Path as Path exposing (Path)
import TreeEdit.Selection as Selection exposing (Selection)
import TreeEdit.Msg as Msg exposing (Msg(..))
import TreeEdit.Index as Index

import TreeEdit.Utils as Utils exposing (fromJust)
import TreeEdit.ViewUtils exposing (onClick, blockAll)

import TreeEdit.ContextMenuTypes as ContextMenuTypes
import TreeEdit.ContextMenu as ContextMenu

-- TODO: use html.keyed for speed

-- XXX: Seems to belong better in Tree or Index, but it's really a view
-- function (about presentation) so it lives here.  Of course, so is
-- terminalString, but that needs to know about internal types of Tree, so it
-- lives there.
labelText : Tree -> String
labelText tree =
    let
        index = (.getOption Tree.index) tree
        label = tree.label
    in
        index |>
        Maybe.map Index.string |>
        Maybe.withDefault "" |>
        (++) label

blockAll : Ev.Options
blockAll = { stopPropagation = True
           , preventDefault = True
           }

decodeMouse : Json.Decoder ContextMenuTypes.Position
decodeMouse = Json.map2 (\x y -> { x = x, y = y })
              (Json.field "x" Json.int)
              (Json.field "y" Json.int)

snode : Path -> Tree -> Bool -> List (Html Msg) -> Html Msg
snode self tree selected children =
    let
        rightClick = Ev.onWithOptions "contextmenu" blockAll <|
                     Json.map (\x -> RightClick self x) decodeMouse
    in
        div
        [ Attr.class "snode"
        , Attr.style [ ("margin-left", "20px")
                     , ("border", "1px solid silver")
                     , ("border-left", "4px solid #4682B4")
                     , ("background-color", if selected
                                            then "#4682B4"
                                            else "#EFEFEF")
                     , ("padding", "2px")
                     , ("cursor", "pointer")
                     , ("color", "black")
                     ]
        , onClick <| ToggleSelect self
        , rightClick
        ] <| text (labelText tree) :: children

wnode : Tree -> Html Msg
wnode t = span
             [ Attr.class "wnode"
             , Attr.style [ ("margin-left", "20px")
                          , ("padding-left", "4px")
                          , ("padding-right", "4px")
                          , ("border", "1px solid black")
                          , ("background-color", "white")
                          , ("color", "black")
                          ]
             ]
             [text <| Tree.terminalString <| t]

viewTree : List Path -> Path -> Tree -> Html Msg
viewTree selected selfPath tree =
    let
        isSelected = List.member selfPath selected
        viewT d =
            snode selfPath d isSelected [wnode d]
        viewNt d =
            (.getOption Tree.children) tree |> Utils.fromJust |>
            Utils.enumerate |>
            List.map (\(i, c) -> viewTree selected (Path.childPath i selfPath) c) |>
            snode selfPath d isSelected
    in
        Tree.either viewNt viewT tree

viewRootTree : Maybe (List Path) -> Int -> Tree -> Html Msg
viewRootTree selected selfIndex tree = viewTree (Maybe.withDefault [] selected) (Path.singleton selfIndex) tree

-- Why do we go through all these contortions?  Html.lazy compares based on
-- equality of reference, not equality per se (source:
-- <https://groups.google.com/forum/#!topic/elm-discuss/AFnzfJtzfRA>).  Only
-- primitive types and a few others (e.g. Nothing, I think) are necesarily
-- referentially equal if they are also equal values.  The first problematic
-- case is the Path that we construct from the enumeration index, so we have a
-- special top-layer view function that takes an Int and constructs the path.
-- The second problem is the list of selected paths: we condense this to
-- Nothing if 1) nothing is selected or 2) the selection is not inside the
-- current tree.  There might be further performance optimizations we could
-- make here, but for now it's Good Enough™
viewRoot : Model -> List (Html Msg)
viewRoot model =
    let
        selectedTrees1 = Selection.get model.selected
        selectedTrees = if selectedTrees1 == [] then Nothing else Just selectedTrees1
    in
        (.get Model.root model) |>
        (.getOption Tree.children) |>
        Utils.fromJust |>
        Utils.enumerate |>
        List.map (\(i, c) ->
                      let
                          sel = if Just True == Maybe.map
                                (\x -> List.member (Path.singleton i) (List.map Path.root x))
                                selectedTrees
                                then selectedTrees
                                else Nothing
                      in
                          lazy3 viewRootTree sel i c)

-- TODO: ame name
viewRoot1 : Model -> Html Msg
viewRoot1 m =
    viewRoot m |>
    div [ Attr.class "sn0"
        , Attr.style [ ("background-color", "#D2B48C")
                     , ("border", "1px solid black")
                     , ("margin-left", "10%")
                     , ("margin-right", "5%")
                     , ("display", "inline-block")
                     ]
        ]

view : Model -> Html Msg
view model =
    div [] [ viewRoot1 model
           , map Msg.Context <| ContextMenu.view model Model.contextMenu
           ]
