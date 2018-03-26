module TreeEdit.Metadata.Type exposing ( Metadata
                                       , MetadataForm
                                       , FieldState(..)
                                       , FieldStates
                                       , Msg(..)
                                       )

import Dict exposing (Dict)
import Keyboard.Event exposing (KeyboardEvent)
import Form exposing (Form)
import RemoteData exposing (WebData)

type alias Metadata = Dict String String

type alias MetadataForm = Form () Metadata

type FieldState = Hidden | Visible Bool

type alias FieldStates = Dict String FieldState

type Msg = ReceivedDefinition (WebData String) |
    Form Form.Msg |
    Edit String |
    Delete String |
    Save |
    Cancel |
    NewSelection |
    SaveSuccess String |
    Key KeyboardEvent
