module TestPath exposing (..)

import Test exposing (..)
import Expect
import Fuzz as F exposing (Fuzzer)

import Maybe.Extra exposing (isJust)

import TreeEdit.Path exposing (..)

p : List Int -> Path
p = List.foldl childPath (singleton "foo")

fuzzPath : Fuzzer Path
fuzzPath = F.map2 internals.path F.string <| F.list <| F.intRange 0 50

suite : Test
suite = describe "Path" <|
        [ describe "childPath" <|
              [ test "works in a basic case" <|
                    \() -> Expect.equal (p [1, 2, 0]) <| childPath 0 <| p [1, 2]
              ]
        , describe "daughterOf" <|
            [ test "trivial case" <| \() -> Expect.true "trivial case" <| daughterOf (p [0,1]) (p [0,1,2])
            , test "reversed arguments" <| \() -> Expect.false "reversed arguments" <| daughterOf (p [0,1,2]) (p [0,1])
            , test "elements not in common" <| \() -> Expect.false "elements not in common" <| daughterOf (p [0,1]) (p [2,1])
            , test "irreflexive" <| \() -> Expect.false "irreflexive" <| daughterOf (p [0,1]) (p [0,1])
            , describe "generative tests" <|
                [ fuzz fuzzPath "irreflexive" <| \path -> Expect.false "irreflexive" <| daughterOf path path
                , fuzz (F.tuple ( fuzzPath, F.intRange 0 50 )) "child path" <|
                    \(path, idx) -> Expect.true "child path" <| daughterOf path (childPath idx path)
                ]
            ]
        , describe "subtract" <|
            [ fuzz (F.tuple (fuzzPath, fuzzPath)) "relation with daughterOf" <|
                  \(path1, path2) -> if daughterOf path1 path2
                                     then Expect.true "daughter path can be subtracted" (subtract path1 path2 |> isJust)
                                     else Expect.equal Nothing <| subtract path1 path2
            , fuzz (F.tuple ( fuzzPath, F.list <| F.intRange 0 50 )) "subtracting daughter gives back original path" <|
                \(path, daughters) -> Expect.equal (Just <| List.reverse daughters) <|
                                      Maybe.andThen internals.fragmentChildren <|
                                      subtract path <| List.foldl childPath path daughters
            , fuzz (F.tuple ( fuzzPath, F.list <| F.intRange 0 50 )) "subtract -> join is noop" <|
                \(path, daughters) ->
                    let
                        daughter = List.foldl childPath path daughters
                    in
                        Expect.equal (Just daughter) <| Maybe.map (join path) <| subtract path daughter
            ]
            , describe "commonPrefix" <|
                [ test "basic" <| \() -> Expect.equal (Just <| p [0,1]) <| commonPrefix (p [0,1,2]) (p [0,1,3])
                , test "identical paths" <| \() -> Expect.equal (Just <| p [0,1]) <| commonPrefix (p [0,1]) (p [0,1])
                , test "empty prefix" <| \() -> Expect.equal (Just <| p []) <| commonPrefix (p [0,1,2]) (p [3,4,5])
                , test "left is empty" <| \() -> Expect.equal (Just <| p []) <| commonPrefix (p []) (p [0,1,3])
                , test "right is empty" <| \() -> Expect.equal (Just <| p []) <| commonPrefix (p [0,1,2]) (p [])
                ]
        , describe "calculateMovement" <|
            [ test "basic" <|
            \() -> Expect.equal { siblingFrom = (p [0,1,2])
                                , siblingTo = (p [0,1,3])
                                , tailFrom = (internals.fragment [3])
                                , tailTo = (internals.fragment [4])
                                , adjacency = Right
                                } <|
            calculateMovement (p [0, 1, 2, 3]) (p [0, 1, 3, 4])]
        ]
