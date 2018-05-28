module TestUtils exposing (..)

import Test exposing (..)
import Expect

import Array

import TreeEdit.Utils exposing (..)

suite : Test
suite = describe "Utils"
        [ describe "insert" <|
          [ test "case 1" <|
                \() -> Expect.equal (Array.fromList [5,6,7,8,9]) <|
                insert 1 6 <| Array.fromList [5,7,8,9]
          , test "empty destination" <|
              \() -> Expect.equal (Array.fromList [1]) <|
              insert 0 1 <| Array.empty
          ]
        , describe "insertMany" <|
            [ test "basic case" <|
              \() -> Expect.equal (Array.fromList [5,6,7,8,9]) <|
              insertMany 1 (Array.fromList [6,7]) <| Array.fromList [5,8,9]
            , test "empty destination" <|
              \() -> Expect.equal (Array.fromList [6,7]) <|
              insertMany 1 (Array.fromList [6,7]) <| Array.fromList []
            ]
            , describe "splice" <|
                [ test "basic case" <|
                  \() -> Expect.equal ( Array.fromList [1,2]
                                      , Array.fromList [3,4]
                                      , Array.fromList [5,6]
                                      ) <|
                      splice 2 4 <| Array.fromList [1,2,3,4,5,6]
                , test "empty beginning" <|
                    \() -> Expect.equal ( Array.fromList []
                                        , Array.fromList [1,2,3,4]
                                        , Array.fromList [5,6]
                                        ) <|
                      splice 0 4 <| Array.fromList [1,2,3,4,5,6]
                , test "empty middle" <|
                    \() -> Expect.equal ( Array.fromList [1,2]
                                        , Array.fromList []
                                        , Array.fromList [3,4,5,6]
                                        ) <|
                      splice 2 2 <| Array.fromList [1,2,3,4,5,6]
                , test "empty end" <|
                    \() -> Expect.equal ( Array.fromList [1,2]
                                        , Array.fromList [3,4,5,6]
                                        , Array.fromList []
                                        ) <|
                      splice 2 6 <| Array.fromList [1,2,3,4,5,6]
                ]
        ]
