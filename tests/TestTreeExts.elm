module TestTreeExts exposing (..)

import Test exposing (..)
import Expect

import MultiwayTree as T

import TreeExts as TX

type alias TestTree = T.Tree String

t s c = T.Tree s c

l s = T.Tree s []

suite : Test
suite = describe "Tree extensions"
        [ describe "setChild"
              [ test "works in a basic case" <|
              \() ->
                  let
                      tree = t "foo" [l "bar", l "baz", l "quux"]
                  in
                      Expect.equal (Just tree) <| TX.setChild 1 (l "baz") tree

              , test "works for first child" <|
                  \() ->
                      let
                          tree = t "foo" [l "bar", l "baz", l "quux"]
                      in
                          Expect.equal (Just tree) <| TX.setChild 0 (l "bar") tree
              , test "works for last child" <|
                  \() ->
                      let
                          tree = t "foo" [l "bar", l "baz", l "quux"]
                      in
                          Expect.equal (Just tree) <| TX.setChild 2 (l "quux") tree
              ]
        ]
