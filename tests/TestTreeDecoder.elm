module TestTreeDecoder exposing (..)

import Test exposing (..)
import Expect

import Json.Decode as D

import TreeEdit.Tree as T exposing (t, l)
import TreeEdit.Tree.Decoder exposing (internals)

{ decode } = internals

suite : Test
suite = describe "decode" <|
        [ test "basic case" <|
        \() -> Expect.equal (Ok (t "IP-MAT"
                                     [ t "NP-SBJ"
                                           [ T.l "D" "the"
                                           , T.l "N" "dog"
                                           ]
                                     , T.l "VBD" "barked"
                                     ])) <|
               D.decodeString decode """
                                      {"label": "IP-MAT", "metadata": {}, "children": [{"label": "NP-SBJ", "metadata": {}, "children": [{"label": "D", "text": "the", "metadata": {}}, {"label": "N", "text": "dog", "metadata": {}}]}, {"label": "VBD", "text": "barked", "metadata": {}}]}
                                      """
            ]
