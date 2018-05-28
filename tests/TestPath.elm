module TestPath exposing (..)

import Test exposing (..)
import Expect

import TreeEdit.Path exposing (..)

p = internals.fromList
l = internals.toList

suite : Test
suite = describe "Path" <|
        [ describe "childPath" <|
              [ test "works in a basic case" <|
                    \() -> Expect.equal (p [1, 2, 0]) <| childPath 1 <| p [2, 0]
              ]
        ]
