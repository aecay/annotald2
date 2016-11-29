module Tests exposing (all)

import Test exposing (Test, concat)

import TestTreeExts


all : Test
all =
    concat [TestTreeExts.suite]
