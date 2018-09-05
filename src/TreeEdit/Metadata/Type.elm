module TreeEdit.Metadata.Type exposing ( Metadata
                                       , MetadataForm
                                       , FieldState(..)
                                       , FieldStates
                                       , Msg(..)
                                       , Model
                                       , Lemma
                                       )

import Dict exposing (Dict)
import ThirdParty.KeyboardEvent exposing (KeyboardEvent)
import Form exposing (Form)
import RemoteData exposing (WebData)
import Select

type alias Metadata = Dict String String

type alias MetadataForm = Form () Metadata

type FieldState = Hidden | Visible Bool

type alias FieldStates = Dict String FieldState

type alias Lemma = { original : String
                   , normalized : String
                   }

type alias Model = { form : MetadataForm
                   , fieldStates : FieldStates
                   , lemmaSelectState : Select.State
                   , lemmata : List Lemma
                   }

type Msg = ReceivedDefinition (WebData String) |
    Form Form.Msg |
    Edit String |
    Delete String |
    Save |
    Cancel |
    NewSelection |
    SaveSuccess String |
    Key KeyboardEvent |
    LemmaSelect (Select.Msg Lemma)
