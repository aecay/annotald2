module TreeEdit.Model exposing
    ( Model
    , freshUuid
    , init
    )

import Monocle.Lens exposing (Lens, modify)
import Monocle.Optional exposing (Optional)
import Random exposing (Seed)
import RemoteData exposing (RemoteData(..))
import ThirdParty.Uuid as Uuid
import TreeEdit.Config as Config
import TreeEdit.Metadata.Type exposing (Lemma)
import TreeEdit.Model.Type exposing (ForestModel)
import TreeEdit.OrderedDict as OD
import TreeEdit.Selection as Selection
import TreeEdit.Tree.Type exposing (Forest)
import TreeEdit.View.LabelEdit.Type exposing (LabelForm)

type alias Model =
    TreeEdit.Model.Type.Model


init : String -> Seed -> Model
init filename seed =
    { webdata = NotAsked
    , lastMessage = "Init"
    , fileName = filename
    , dialog = Nothing
    , dirty = False
    , seed = seed
    }


freshUuid : ForestModel -> ( ForestModel, String )
freshUuid m =
    let
        ( newId, newSeed ) =
            Random.step Uuid.generator m.seed
    in
    ( { m | seed = newSeed }, newId )
