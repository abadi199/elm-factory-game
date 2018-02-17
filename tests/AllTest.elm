module AllTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import ProjectorTest
import Test exposing (..)


suite : Test
suite =
    describe "All Tests"
        [ ProjectorTest.suite ]
