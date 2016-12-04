module Main exposing (..)

import App exposing (..)
import Navigation exposing (program)
import Types exposing (Model, Msg(UrlChange))


main : Program Never Model Msg
main =
    program UrlChange { view = view, init = init, update = update, subscriptions = subscriptions }
