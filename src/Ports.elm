port module Ports exposing (..)

import Types exposing (Session)


-- saveSession makes javascript persist our session info in localstorage
-- when the app boots up this session is passed in via flags.


port saveSession : Session -> Cmd msg



-- outbound ports have to take parameters (thanks Obama)
-- so for logout we make the parameter the unit type ()


port logout : () -> Cmd msg
