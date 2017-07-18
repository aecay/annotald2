module TestPath exposing (..)

import Test exposing (..)
import Expect

suite : Test
suite = skip <| test "nothing yet" <| \() -> Expect.true "foo" True
--describe "Path"
        -- [ describe "removeCommon" <|
        --       [ skip "works in a basic case" <|
        --             \() -> Expect.equal ([1,2], [3,4], [5,6]) <| removeCommon [1,2,3,4] [1,2,5,6]
        --       , skip "works with left arg empty" <|
        --           \() -> Expect.equal ([], [], [1,2]) <| removeCommon [] [1,2]
        --       , skip "works with right arg empty" <|
        --           \() -> Expect.equal ([], [1,2], []) <| removeCommon [1,2] []
        --       , skip "works when no common part" <|
        --           \() -> Expect.equal ([], [1,2], [3,4]) <| removeCommon [1,2] [3,4]
        --       ]
        -- ]
