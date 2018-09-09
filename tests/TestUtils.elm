module TestUtils exposing (..)

import Test exposing (..)
import Expect

import Array exposing (Array)

import TreeEdit.Utils exposing (..)

type Foo = Foo (Array Int)

coll : List a -> Array a
coll = Array.fromList

empty : Array a
empty = coll []

suite : Test
suite = describe "Utils"
        [ describe "insert" <|
          [ test "basic case" <|
                \() -> Expect.equal (coll [5,6,7,8,9]) <|
                insert 1 6 <| coll [5,7,8,9]
          , test "empty destination" <|
              \() -> Expect.equal (coll [1]) <|
              insert 0 1 <| empty
          , test "insert beyond the end" <|
              \() -> Expect.equal (coll [1,2,3]) <| insert 10 3 <| coll [1,2]
          ]
        , describe "insertMany" <|
            [ test "basic case" <|
              \() -> Expect.equal (coll [5,6,7,8,9]) <|
              insertMany 1 (coll [6,7]) <| coll [5,8,9]
            , test "empty destination" <|
              \() -> Expect.equal (coll [6,7]) <|
              insertMany 1 (coll [6,7]) <| empty
            ]
            , describe "splice" <|
                [ test "basic case" <|
                  \() -> Expect.equal ( coll [1,2]
                                      , coll [3,4]
                                      , coll [5,6]
                                      ) <|
                      splice 2 4 <| coll [1,2,3,4,5,6]
                , test "empty beginning" <|
                    \() -> Expect.equal ( coll []
                                        , coll [1,2,3,4]
                                        , coll [5,6]
                                        ) <|
                      splice 0 4 <| coll [1,2,3,4,5,6]
                , test "empty middle" <|
                    \() -> Expect.equal ( coll [1,2]
                                        , coll []
                                        , coll [3,4,5,6]
                                        ) <|
                      splice 2 2 <| coll [1,2,3,4,5,6]
                , test "empty end" <|
                    \() -> Expect.equal ( coll [1,2]
                                        , coll [3,4,5,6]
                                        , coll []
                                        ) <|
                      splice 2 6 <| coll [1,2,3,4,5,6]
                ]
        , describe "findPrefix" <|
            [ test "basic case" <| \() -> Expect.equal [1,2,3] <| internals.findPrefix [1,2,3,4] [1,2,3,5]
            , test "first is shorter" <| \() -> Expect.equal [1,2,3] <| internals.findPrefix [1,2,3] [1,2,3,5]
            , test "second is shorter" <| \() -> Expect.equal [1,2,3] <| internals.findPrefix [1,2,3,4] [1,2,3]
            , test "both equal" <| \() -> Expect.equal [1,2,3] <| internals.findPrefix [1,2,3] [1,2,3]
            , test "first is empty" <| \() -> Expect.equal [] <| internals.findPrefix [] [1,2,3,5]
            , test "second is empty" <| \() -> Expect.equal [] <| internals.findPrefix [1,2,3,4] []
            , test "both empty" <| \() -> Expect.equal [] <| internals.findPrefix [] []
            ]
        , describe "findSuffix" <|
            [ test "basic case" <| \() -> Expect.equal [3,2,1] <| findSuffix [4,3,2,1] [5,3,2,1]
            , test "first is shorter" <| \() -> Expect.equal [3,2,1] <| findSuffix [3,2,1] [5,3,2,1]
            , test "second is shorter" <| \() -> Expect.equal [3,2,1] <| findSuffix [4,3,2,1] [3,2,1]
            , test "both equal" <| \() -> Expect.equal [3,2,1] <| findSuffix [3,2,1] [3,2,1]
            , test "first is empty" <| \() -> Expect.equal [] <| findSuffix [] [1,2,3,5]
            , test "second is empty" <| \() -> Expect.equal [] <| findSuffix [1,2,3,4] []
            , test "both empty" <| \() -> Expect.equal [] <| findSuffix [] []
            ]
        ]
