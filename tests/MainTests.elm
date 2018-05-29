module MainTests exposing (..)

import Test
import Test.Runner.Html as Runner

import TestIntegration
import TestPath
import TestTreeDecode
import TestTree
import TestUtils

-- Black magic: this module will be picked up as a test iff it has a top-level
-- value of type Test.  We don't want that.  So, we put the "tests" variable
-- inside a let and all is copacetic

main : Runner.TestProgram
main =
    let
        tests = Test.concat [ TestIntegration.suite
                            , TestPath.suite
                            , TestTreeDecode.suite
                            , TestTree.suite
                            , TestUtils.suite
                            ]
    in
        Runner.run tests
