module Main exposing (..)

import App exposing (..)
import Navigation
import Types exposing (Model, Msg(UrlChange), Flags)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange { view = view, init = init, update = update, subscriptions = subscriptions }
