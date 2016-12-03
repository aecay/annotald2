module Tests exposing (all)

import Test exposing (Test, concat)

import TestTreeExts
import TestTree


all : Test
all =
    concat [TestTreeExts.suite, TestTree.suite]
