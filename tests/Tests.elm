module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Data.RemoteData as RemoteData exposing (RemoteData(Success))


all : Test
all =
    describe "Tests"
        [ describe "RemoteData"
            [ test "RemoteData.mapSuccess" <|
                \() ->
                    Expect.equal (RemoteData.mapSuccess ((+) 2) (Success 3)) (Success 5)
            ]
        ]
