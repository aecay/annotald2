port module TreeView exposing (main)

import Browser
import Dict
import Html exposing (Html)
import Json.Decode as D exposing (decodeString)
import Platform.Cmd as Cmd
import Platform.Sub as Sub

import TreeEdit.Path as Path
import TreeEdit.Tree.Type exposing (Tree)
import TreeEdit.Tree as Tree
import TreeEdit.Tree.Decode exposing (decodeTree)
import TreeEdit.View exposing (viewTree, ViewInfo)

type alias Model = { tree : Result D.Error Tree, viewInfo: ViewInfo }

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

type Msg = TreeChanged (Result D.Error Tree) |
    Noop

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TreeChanged t -> ( { model | tree = t }, Cmd.none )
        Noop -> ( model, Cmd.none )

view : Model -> Html Msg
view {tree, viewInfo} =
    case tree of
        Err msg -> Html.div [] [ Html.text <| "Error deserializing tree: " ++ D.errorToString msg ]
        Ok innerTree ->
            let
                go i t = viewTree viewInfo (Path.singleton i) t |>
                         Html.map (\_ -> Noop)
                id = .get Tree.metadata innerTree |> Dict.get "ID"
            in
                case id of
                    Just realId -> go realId innerTree
                    Nothing ->
                        let
                            newMetadata = .get Tree.metadata innerTree |> Dict.update "ID" (\_ -> Just "FOO")
                        in
                            go "FOO" <| .set Tree.metadata newMetadata innerTree


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        doTreeChg x = decodeString decodeTree x
    in
        treeChanged <| TreeChanged << doTreeChg

main : Program Flags Model Msg
main = Browser.element
       { init = init
       , update = update
       , view = view
       , subscriptions = subscriptions
       }
