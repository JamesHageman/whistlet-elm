port module Ports exposing (..)

import Types exposing (Session)


port saveSession : Session -> Cmd msg
