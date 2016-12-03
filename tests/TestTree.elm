module TestTree exposing (..)

import Test exposing (..)
import Expect

import Tree as T

t = T.t

l s = T.l s ""

{ allLast, canMove, isLastAt, removeCommon, extractAt, fixPathForMovt } = T.internals

suite : Test
suite = describe "Tree"
        [ describe "removeCommon" <|
              [ test "works in a basic case" <|
                    \() -> Expect.equal ([1,2], [3,4], [5,6]) <| removeCommon [1,2,3,4] [1,2,5,6]
              , test "works with left arg empty" <|
                  \() -> Expect.equal ([], [], [1,2]) <| removeCommon [] [1,2]
              , test "works with right arg empty" <|
                  \() -> Expect.equal ([], [1,2], []) <| removeCommon [1,2] []
              , test "works when no common part" <|
                  \() -> Expect.equal ([], [1,2], [3,4]) <| removeCommon [1,2] [3,4]
              ]
        , describe "isLastAt" <|
            [ test "case one" <|
                  \() -> Expect.equal True <| flip isLastAt [0] <|
                         t "x"
                             [ t "x"
                                   [ l "x"
                                   , l "x"
                                   , t "x"
                                       [ l "x"
                                       , l "x"
                                       ]
                                   ]
                             ]
            , test "case two" <|
                  \() -> Expect.equal True <| flip isLastAt [0, 2] <|
                         t "x"
                             [ t "x"
                                   [ l "x"
                                   , l "x"
                                   , t "x"
                                       [ l "x"
                                       , l "x"
                                       ]
                                   ]
                             ]
            , test "case three" <|
                  \() -> Expect.equal True <| flip isLastAt [0, 2, 1] <|
                         t "x"
                             [ t "x"
                                   [ l "x"
                                   , l "x"
                                   , t "x"
                                       [ l "x"
                                       , l "x"
                                       ]
                                   ]
                             ]
            , test "from the test program" <|
                \() -> Expect.equal True <| flip isLastAt [0,1] <|
                       t "IP-MAT"
                           [ t "NP-SBJ"
                                 [ l "the"
                                 , l "dog" ]
                           , l "barked"
                           ]
            ]
        , describe "allLast" <|
            [ test "works in a basic case" <|
                  \() -> Expect.equal True <| allLast [0] [2, 1] <|
                  t "x"
                      [ t "x"
                            [ l "x"
                            , l "x"
                            , t "x"
                                [ l "x"
                                , l "x"
                                ]
                            ]
                      ]
            , test "detects negatives" <|
                \() -> Expect.equal False <| allLast [0] [2, 0] <|
                       t "x"
                           [ t "x"
                                 [ l "x"
                                 , l "x"
                                 , t "x"
                                     [ l "x"
                                     , l "x"
                                     ]
                                 ]
                           ]
            , test "works when the list of children is too long" <|
                \() -> Expect.equal False <| allLast [0] [2,1,3] <|
                       t "x"
                           [ t "x"
                                 [ l "x"
                                 , l "x"
                                 , t "x"
                                     [ l "x"
                                     , l "x"
                                     ]
                                 ]
                           ]
            , test "a case from below" <|
                \() -> Expect.equal True <| allLast [0] [2] <|
                       t "x"
                           [ t "y"
                                 [ l "a"
                                 , l "b"
                                 , l "c"
                                 ]
                           , t "z"
                               [ l "1"
                               , l "2"
                               , l "3"
                               ]
                           ]
            , test "from the test program" <|
                \() -> Expect.equal True <| allLast [0] [1] <|
                       t "IP-MAT"
                           [ t "NP-SBJ"
                                 [ l "the"
                                 , l "dog" ]
                           , l "barked"
                           ]
            ]
        , describe "canMove" <|
            [ test "works in a basic case" <|
                  \() -> Expect.equal True <| canMove [0, 2] [1, 0] <|
                         t "x"
                             [ t "y"
                                   [ l "a"
                                   , l "b"
                                   , l "c"
                                   ]
                             , t "z"
                                 [ l "1"
                                 , l "2"
                                 , l "3"
                                 ]
                             ]
            , test "from the test program" <|
                \() -> Expect.equal True <| canMove [1] [0,2] <|
                       t "IP-MAT"
                           [ t "NP-SBJ"
                                 [ l "the"
                                 , l "dog" ]
                           , l "barked"
                           ]
            , test "from the test program 2" <|
                \() -> Expect.equal True <| canMove [0,1] [1] <|
                       t "IP-MAT"
                           [ t "NP-SBJ"
                                 [ l "the"
                                 , l "dog" ]
                           , l "barked"
                           ]
            ]
        , describe "moveTo" <|
            [ test "works in a basic case" <|
                  \() -> Expect.equal (Just (t "x"
                                                [ t "y"
                                                      [ l "a"
                                                      , l "b"
                                                      ]
                                                , t "z"
                                                    [ l "c"
                                                    , l "1"
                                                    , l "2"
                                                    , l "3"
                                                    ]
                                                ])) <|
                   T.moveTo [0, 2] [1, 0] <|
                      t "x"
                          [ t "y"
                                [ l "a"
                                , l "b"
                                , l "c"
                                ]
                          , t "z"
                              [ l "1"
                              , l "2"
                              , l "3"
                              ]
                          ]
            , test "refuses illegal movement" <|
                \() -> Expect.equal Nothing <|
                       T.moveTo [0, 1] [1, 0] <|
                           t "x"
                               [ t "y"
                                     [ l "a"
                                     , l "b"
                                     , l "c"
                                     ]
                               , t "z"
                                   [ l "1"
                                   , l "2"
                                   , l "3"
                                   ]
                          ]
            , test "from the test program" <|
                \() -> Expect.equal (Just <| t "IP-MAT"
                                         [ t "NP-SBJ"
                                               [ l "the"
                                               , l "dog"
                                               , l "barked"
                                               ]
                                         ]) <|
                       T.moveTo [1] [0,2] <|
                           t "IP-MAT"
                               [ t "NP-SBJ"
                                     [ l "the"
                                     , l "dog" ]
                               , l "barked"
                               ]
            , test "disallows moving only child" <|
                \() -> Expect.equal Nothing <|
                       T.moveTo [1,0] [0,3] <|
                           t "IP"
                               [ t "NP"
                                     [ l "the"
                                     , l "dog"
                                     ]
                               , t "VP"
                                   [ l "barked"
                                   ]
                               ]
            ]
            , describe "extractAt" <|
                [ test "baisc case" <|
                      \() -> Expect.equal (Just (l "barked",
                                                 t "IP-MAT"
                                                     [ t "NP-SBJ"
                                                       [ l "the"
                                                       , l "dog"
                                                       ]
                                                     ]
                                                )) <|
                             extractAt [1] (t "IP-MAT"
                                                [ t "NP-SBJ"
                                                      [ l "the"
                                                      , l "dog" ]
                                                , l "barked"
                                                ])
                ]
        , describe "fixPathForMovt" <|
            [ test "basic case" <|
                  \() -> Expect.equal [0,0,1] <| fixPathForMovt [0,0] [0,1,1]
            ]
        ]
