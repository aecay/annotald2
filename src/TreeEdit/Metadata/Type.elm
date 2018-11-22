module TreeEdit.Metadata.Type exposing
    ( FieldState(..)
    , FieldStates
    , Lemma
    , Metadata
    , MetadataForm
    , Model
    , Msg(..)
    )

import Dict exposing (Dict)
import Form exposing (Form)
import RemoteData exposing (WebData)
import Select
import ThirdParty.KeyboardEvent exposing (KeyboardEvent)


type alias Metadata =
    Dict String String


type alias MetadataForm =
    Form () Metadata


type FieldState
    = Hidden
    | Visible Bool


type alias FieldStates =
    Dict String FieldState


type alias Lemma =
    { original : String
    , normalized : String
    }


type alias Model =
    { form : MetadataForm
    , fieldStates : FieldStates
    , lemmaSelectState : Select.State
    , lemmata : List Lemma
    , lemmaInput : Maybe String
    }


type Msg
    = ReceivedDefinition (WebData String)
    | Form Form.Msg
    | Edit String
    | Delete String
    | Save
    | Cancel
    | NewSelection
    | SaveSuccess String
    | Key KeyboardEvent
    | LemmaSelect (Select.Msg Lemma)
    | LemmaQueryChanged String
