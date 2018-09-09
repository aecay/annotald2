module TestTree exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)

import Dict
import Json.Decode as D
import Tuple

import TreeEdit.Result as R
import TreeEdit.Tree as T
import TreeEdit.Tree.Type as TreeType exposing (Tree, Forest)
import TreeEdit.Path as Path exposing (Path)

p : List Int -> Path
p = .path Path.internals "foo"
pf = .fragment Path.internals

t = .t TreeType.private
l s = .l TreeType.private s ""

forest : Tree -> Forest
forest tree =
    let
        withId = .set T.metadata (Dict.fromList [("ID", "foo")]) tree
    in
        T.forestFromList [ withId ]


allLast = .allLast T.internals
isLastAt = .isLastAt T.internals
deleteAt = .deleteAt T.internals
insertAt = .insertAt T.internals
-- { allLast, isLastAt, deleteAt, insertAt } = T.internals

err : R.Result a -> Expectation
err x = case x of
            R.Result _ _ Nothing -> Expect.pass
            _ -> Expect.fail "expected an error result"

ok : a -> R.Result a -> Expectation
ok val res =
    let
        (R.Result msgs _ wrapped) = res
    in
        case wrapped of
            Nothing -> Expect.fail <| "Expected a passing result, but failed with messages " ++ Debug.toString msgs
            Just v -> Expect.equal v val

-- equalTree : Tree -> Tree -> Expectation
-- equalTree a b =
--     Expect.all [ Expect.equal (.get T.info a) (.get T.info b)
--                ,
--                ]

suite : Test
suite = describe "Tree"
        [ describe "isLastAt" <|
            [ test "case one" <|
                  \() -> Expect.equal True <| (\x -> isLastAt x (p [0])) <| forest <|
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
                  \() -> Expect.equal True <| (\x -> isLastAt x (p [2,0])) <| forest <|
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
                  \() -> Expect.equal True <| (\x -> isLastAt x (p [1, 2, 0])) <| forest <|
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
                \() -> Expect.equal True <| (\x -> isLastAt x (p [1, 0])) <| forest <|
                       t "IP-MAT"
                           [ t "NP-SBJ"
                                 [ l "the"
                                 , l "dog" ]
                           , l "barked"
                           ]
            ]
        , describe "allLast" <|
            [ test "works in a basic case" <|
                  \() -> Expect.equal True <| allLast (p [0]) (pf [1, 2]) <| forest <|
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
                \() -> Expect.equal False <| allLast (p [0]) (pf [0, 2]) <| forest <|
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
                \() -> Expect.equal True <| allLast (p [0]) (pf [2]) <| forest <|
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
                \() -> Expect.equal True <| allLast (p [0]) (pf [1]) <| forest <|
                       t "IP-MAT"
                           [ t "NP-SBJ"
                                 [ l "the"
                                 , l "dog" ]
                           , l "barked"
                           ]
            , test "default is true" <|
                \() -> Expect.equal True <| allLast (p [0]) (pf []) <| forest <| t "foo" [ l "bar" ]
            ]
        , describe "moveTo" <|
            [ test "works in a basic case" <|
            \() -> ok (forest <| t "x"
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
                       ], p [0,1]) <|
                   T.moveTo (p [2, 0]) (p [1]) <| forest <|
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
                \() -> err <|
                       T.moveTo (p [1, 0]) (p [0, 1]) <| forest <|
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
                \() -> ok (forest <| t "IP-MAT"
                           [ t "NP-SBJ"
                                 [ l "the"
                                 , l "dog"
                                 , l "barked"
                                 ]
                           ]) <| R.map Tuple.first <|
                       T.moveTo (p [1]) (p [0]) <| forest <|
                           t "IP-MAT"
                               [ t "NP-SBJ"
                                     [ l "the"
                                     , l "dog" ]
                               , l "barked"
                               ]
            , test "disallows moving only child" <|
                \() -> err <|
                       T.moveTo (p [0, 1]) (p [3, 0]) <| forest <|
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
            , describe "deleteAt" <|
                [ test "basic case" <|
                \() -> Expect.equal (forest <| t "IP-MAT"
                                         [ t "NP-SBJ"
                                               [ l "the"
                                               , l "dog"
                                               ]
                                         ]
                                    ) <|
                             deleteAt (p [1]) <| forest <|
                                 (t "IP-MAT"
                                      [ t "NP-SBJ"
                                            [ l "the"
                                            , l "dog" ]
                                      , l "barked"
                                      ])
                , test "at 0" <|
                    \() -> Expect.equal (forest <| t "foo" [ l "one"
                                                 , l "two"
                                                 ]) <|
                           deleteAt (p [0]) <| forest <|
                               t "foo" [ l "zero"
                                       , l "one"
                                       , l "two"
                                       ]
                , test "at end" <|
                    \() -> Expect.equal (forest <| t "foo" [ l "zero"
                                                     , l "one"
                                                     ]) <|
                           deleteAt (p [2]) <| forest <|
                               t "foo" [ l "zero"
                                       , l "one"
                                       , l "two"
                                       ]
                , test "nested at 0" <|
                    \() -> Expect.equal (forest <| t "bar" [ l "a"
                                                 , t "foo" [ l "one"
                                                           , l "two"
                                                           ]
                                                 , l "b"
                                                 ]) <|
                           deleteAt (p [0, 1]) <| forest <|
                               t "bar" [ l "a"
                                       , t "foo" [ l "zero"
                                                 , l "one"
                                                 , l "two"
                                                 ]
                                       , l "b"
                                       ]
                , test "nested at end" <|
                    \() -> Expect.equal (forest <| t "bar" [ l "a"
                                                 , t "foo" [ l "zero"
                                                           , l "one"
                                                           ]
                                                 , l "b"
                                                 ]) <|
                           deleteAt (p [2, 1]) <| forest <|
                               t "bar" [ l "a"
                                       , t "foo" [ l "zero"
                                                 , l "one"
                                                 , l "two"
                                                 ]
                                       , l "b"
                                       ]
                ]
        , describe "insertAt" <|
            [ test "at 0" <|
            \() -> Expect.equal (forest <| t "foo" [ l "zero"
                                               , l "one"
                                               , l "two"
                                               ])  <|
                      insertAt (p [0]) (l "zero") <| forest <|
                      t "foo" [ l "one"
                              , l "two"
                              ]
                , test "at end" <|
                    \() -> Expect.equal (forest <| t "foo" [ l "zero"
                                                 , l "one"
                                                 , l "two"
                                                 ]) <|
                           insertAt (p [2]) (l "two") <| forest <|
                               t "foo" [ l "zero"
                                       , l "one"
                                       ]
                , test "nested at 0" <|
                    \() -> Expect.equal (forest <| t "bar" [ l "a"
                                                 , t "foo" [ l "zero"
                                                           , l "one"
                                                           , l "two"
                                                           ]
                                                 , l "b"
                                                 ]) <|
                           insertAt (p [0, 1]) (l "zero") <| forest <|
                               t "bar" [ l "a"
                                       , t "foo" [ l "one"
                                                 , l "two"
                                                 ]
                                       , l "b"
                                       ]
                , test "nested at end" <|
                    \() -> Expect.equal (forest <| t "bar" [ l "a"
                                                 , t "foo" [ l "zero"
                                                           , l "one"
                                                           , l "two"
                                                           ]
                                                 , l "b"
                                                 ]) <|
                           insertAt (p [2, 1]) (l "two") <| forest <|
                               t "bar" [ l "a"
                                       , t "foo" [ l "zero"
                                                 , l "one"
                                                 ]
                                       , l "b"
                                       ]
                ]
        , describe "get" <|
            [ test "basic" <|
            \() -> Expect.equal (l "zero") <| T.get (p [0]) <|
                   (forest <| t "foo" [ l "zero"
                                      , l "one"
                                      , l "two"
                                      ])
            , test "" <|
            \() ->
                let
                    tree = t "foo" [ l "zero" , l "one" , l "two"]
                in
                    Expect.equal t  <| T.get (p []) <| forest t
            ]
        ]
