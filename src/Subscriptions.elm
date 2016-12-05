module Subscriptions exposing (subscriptions)

import Types exposing (Model, Msg(TimeUpdate))
import Time


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (30.0 * Time.second) TimeUpdate
