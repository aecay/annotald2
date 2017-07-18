module TestTree exposing (..)

import Test exposing (..)
import Expect

import Json.Decode as D

import TreeEdit.Tree as T
import TreeEdit.Path

p = .fromList TreeEdit.Path.internals
pf = .pf TreeEdit.Path.internals

t = T.t

l s = T.l s ""

{ allLast, canMove, isLastAt, extractAt, fixPathForMovt } = T.internals

suite : Test
suite = describe "Tree"
        [ describe "isLastAt" <|
            [ test "case one" <|
                  \() -> Expect.equal True <| flip isLastAt (p [0]) <|
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
                  \() -> Expect.equal True <| flip isLastAt (p [2,0]) <|
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
                  \() -> Expect.equal True <| flip isLastAt (p [1, 2, 0]) <|
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
                \() -> Expect.equal True <| flip isLastAt (p [1, 0]) <|
                       t "IP-MAT"
                           [ t "NP-SBJ"
                                 [ l "the"
                                 , l "dog" ]
                           , l "barked"
                           ]
            ]
        , describe "allLast" <|
            [ test "works in a basic case" <|
                  \() -> Expect.equal True <| allLast (p [0]) (pf [1, 2]) <|
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
                \() -> Expect.equal False <| allLast (p [0]) (pf [0, 2]) <|
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
                \() -> Expect.equal False <| allLast (p [0]) (pf [3, 1, 2]) <|
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
                \() -> Expect.equal True <| allLast (p [0]) (pf [2]) <|
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
                \() -> Expect.equal True <| allLast (p [0]) (pf [1]) <|
                       t "IP-MAT"
                           [ t "NP-SBJ"
                                 [ l "the"
                                 , l "dog" ]
                           , l "barked"
                           ]
            ]
        , describe "canMove" <|
            [ test "works in a basic case" <|
                  \() -> Expect.equal True <| canMove (p [2, 0]) (p [0, 1]) <|
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
                \() -> Expect.equal True <| canMove (p [1]) (p [2, 0]) <|
                       t "IP-MAT"
                           [ t "NP-SBJ"
                                 [ l "the"
                                 , l "dog" ]
                           , l "barked"
                           ]
            , test "from the test program 2" <|
                \() -> Expect.equal True <| canMove (p [1, 0]) (p [1]) <|
                       t "IP-MAT"
                           [ t "NP-SBJ"
                                 [ l "the"
                                 , l "dog" ]
                           , l "barked"
                           ]
            ]
        , describe "moveTo" <|
            [ test "works in a basic case" <|
                  \() -> Expect.equal (Ok (t "x"
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
                   T.moveTo (p [2, 0]) (p [0, 1]) <|
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
                \() -> Expect.err <|
                       T.moveTo (p [1, 0]) (p [0, 1]) <|
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
                \() -> Expect.equal (Ok <| t "IP-MAT"
                                         [ t "NP-SBJ"
                                               [ l "the"
                                               , l "dog"
                                               , l "barked"
                                               ]
                                         ]) <|
                       T.moveTo (p [1]) (p [2, 0]) <|
                           t "IP-MAT"
                               [ t "NP-SBJ"
                                     [ l "the"
                                     , l "dog" ]
                               , l "barked"
                               ]
            , test "disallows moving only child" <|
                \() -> Expect.err <|
                       T.moveTo (p [0, 1]) (p [3, 0]) <|
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
                      \() -> Expect.equal (Ok (l "barked",
                                                 t "IP-MAT"
                                                     [ t "NP-SBJ"
                                                       [ l "the"
                                                       , l "dog"
                                                       ]
                                                     ]
                                                )) <|
                             extractAt (p [1]) (t "IP-MAT"
                                                [ t "NP-SBJ"
                                                      [ l "the"
                                                      , l "dog" ]
                                                , l "barked"
                                                ])
                ]
        , describe "fixPathForMovt" <|
            [ test "basic case" <|
                  \() -> Expect.equal (p [1, 0, 0]) <| fixPathForMovt (p [0,0]) (p [1, 1, 0])
            ]
        , describe "destPath"
            [ test "moving to parent" <|
                  \() -> Expect.equal (Ok (p [1])) <|
                  T.destPath (p [0, 1]) TreeEdit.Path.RootPath <|
                  t "WTF" [ t "foo" [l "bar", l "baz"]
                          , t "foo2" [l "bar2", l "baz2"]
                          ]

            ]
        ]
