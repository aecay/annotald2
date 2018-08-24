port module TreeView exposing (main)

import Html exposing (Html)
import Json.Decode as D exposing (decodeString)
import Platform.Cmd as Cmd
import Platform.Sub as Sub

import TreeEdit.Path as Path
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Tree.Decode exposing (decodeTree)
import TreeEdit.View exposing (viewTree, ViewInfo)

type alias Model = { tree : Result String Tree, viewInfo: ViewInfo }

type alias Flags = { tree : String, snodeClass: String, ipClass: String, selectedClass: String, wnodeClass: String }

init : Flags -> (Model, Cmd Msg)
init {tree, snodeClass, selectedClass, ipClass, wnodeClass} =
    let
        -- TODO: hardcoding is not desirable
        config = { ipLabels = ["IP-", "FRAG", "RRC"]
                 , dashTags = []
                 , insertables = []
                 , labelGroups = []
                 }
        viewInfo = { config = config
                   , selected = []
                   , labelForm = Nothing
                   , interactive = False
                   , snodeClass = snodeClass
                   , selectedClass = selectedClass
                   , ipClass = ipClass
                   , wnodeClass = wnodeClass
                   }
    in
        ( { tree = decodeString decodeTree tree
          , viewInfo = viewInfo
          }
        , Cmd.none
        )

port treeChanged : (String -> msg) -> Sub msg

type Msg = TreeChanged (Result String Tree) |
    Noop

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TreeChanged t -> ( { model | tree = t }, Cmd.none )
        Noop -> ( model, Cmd.none )

view : Model -> Html Msg
view {tree, viewInfo} =
    case tree of
        Err msg -> Html.div [] [ Html.text <| "Error deserializing tree: " ++ msg ]
        Ok tree -> viewTree viewInfo (Path.singleton 0) tree |>
                   Html.map (\_ -> Noop)

subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        doTreeChg x = decodeString decodeTree x
    in
        treeChanged <| TreeChanged << doTreeChg

main : Program Flags Model Msg
main = Html.programWithFlags
       { init = init
       , update = update
       , view = view
       , subscriptions = subscriptions
       }
