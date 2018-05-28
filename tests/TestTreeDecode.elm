module TestTreeDecode exposing (..)

import Test exposing (..)
import Expect

import Json.Decode as D

import TreeEdit.Tree.Type as T
import TreeEdit.Tree.Decode exposing (decodeTree)

t = .t T.private
l = .l T.private

suite : Test
suite = describe "decodeTree" <|
        [ test "basic case" <|
        \() -> Expect.equal (Ok (t "IP-MAT"
                                     [ t "NP-SBJ"
                                           [ l "D" "the"
                                           , l "N" "dog"
                                           ]
                                     , l "VBD" "barked"
                                     ])) <|
               D.decodeString decodeTree """
                                      {"label": "IP-MAT", "metadata": {}, "children": [{"label": "NP-SBJ", "metadata": {}, "children": [{"label": "D", "text": "the", "metadata": {}}, {"label": "N", "text": "dog", "metadata": {}}]}, {"label": "VBD", "text": "barked", "metadata": {}}]}
                                      """
            ]
