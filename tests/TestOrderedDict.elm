module TestOrderedDict exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)

import TreeEdit.OrderedDict as OD

d : OD.OrderedDict Int String
d = OD.fromList [ (1, "1")
                , (2, "2")
                , (3, "3")
                ]

suite : Test
suite = describe "OrderedDict" <|
        [ describe "update" <|
          [ test "basic" <|
            \() -> Expect.equal (OD.fromList [ (1, "1") , (2, "2") , (3, "4")]) <|
                OD.update 3 (\x -> Just "4") d
          ]
        ]
