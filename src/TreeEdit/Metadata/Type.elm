module TreeEdit.Metadata.Type exposing ( Metadata
                                       , MetadataForm
                                       , FieldState
                                       , Msg(..)
                                       )

import Dict exposing (Dict)

import Form exposing (Form)
import RemoteData exposing (WebData)

type alias Metadata = { lemma : String, definition : Maybe String }

type alias MetadataForm = Form () Metadata

type alias FieldState = Dict String Bool

type Msg = ReceivedDefinition (WebData String) |
    Form Form.Msg |
    Edit String |
    Save |
    NewSelection
