module TreeEdit.Model exposing ( init
                               , root
                               , config
                               , selected
                               , labelForm
                               , lemmata
                               , Model
                               , freshUuid
                               )

import Random exposing (Seed)

import Monocle.Lens exposing (Lens, modify)
import Monocle.Optional exposing (Optional)
import RemoteData exposing (WebData, RemoteData(..))

import ThirdParty.Uuid as Uuid

import TreeEdit.Config as Config
import TreeEdit.Metadata.Type exposing (Lemma)
import TreeEdit.Tree.Type exposing (Forest)
import TreeEdit.Selection as Selection
import TreeEdit.View.LabelEdit.Type exposing (LabelForm)

import TreeEdit.Model.Type

type alias Model = TreeEdit.Model.Type.Model

init : String -> Seed -> Model
init filename seed = { webdata = NotAsked
                     , selected = Selection.empty
                     , lastMessage = "Init"
                     , contextMenu = Nothing
                     , fileName = filename
                     , metadataForm = Nothing
                     , labelForm = Nothing
                     , dialog = Nothing
                     , undo = []
                     , redo = []
                     , dirty = False
                     , seed = seed
                     }

root : Lens Model Forest
root =
    let
        get m = case m.webdata of
                    Success {root} -> root
                    _ -> Debug.crash "Tried to get the root when no data was loaded"
        set tree m = case m.webdata of
                         Success val -> { m | webdata = Success { val | root = tree } }
                         _ -> Debug.crash "Tried to set the root when no data was loaded"
    in
        Lens get set

config : Model -> Config.Config
config m = case m.webdata of
               Success {config} -> config
               _ -> Debug.crash "Tried to get the config when no data was loaded"

selected : Lens Model Selection.Selection
selected = Lens .selected (\s m -> { m | selected = s })

labelForm : Optional Model LabelForm
labelForm =
    let
        getOption m = m.labelForm
        set form m = { m | labelForm = Just form }
    in
        Optional getOption set

lemmata : Model -> List Lemma
lemmata m =
    case m.webdata of
        Success {lemmata} -> lemmata
        _ -> []

freshUuid : Model -> (Model, String)
freshUuid m =
    let
        (newId, newSeed) = Random.step Uuid.generator m.seed
    in
        ({ m | seed = newSeed }, newId)

-- doRoot : (Tree -> R.Result Tree) -> Model -> Model
-- doRoot = R.modify root
