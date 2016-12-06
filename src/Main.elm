module Main exposing (..)

import Update exposing (update, init)
import View exposing (view)
import Subscriptions exposing (subscriptions)
import Navigation
import Types exposing (Model, Msg(UrlChange), Flags)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange { view = view, init = init, update = update, subscriptions = subscriptions }
