module App exposing (..)

import Html exposing (Html, text, div, form, input, button)
import Html.Attributes exposing (type_, placeholder)
import Html.Events exposing (onSubmit, onInput)
import Tuple exposing (first, second)
import RemoteData exposing (..)
import Types exposing (Model, Msg(..), Username, Password, Session)
import Api
import Http


init : ( Model, Cmd Msg )
init =
    ( { session = NotAsked, loginForm = ( "", "" ) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login username password ->
            { model | session = Loading } ! [ Http.send LoginFinish <| Api.login username password ]

        LoginFinish res ->
            res
                |> Result.map
                    (\session ->
                        { model | session = Success session } ! []
                    )
                |> Result.withDefault (model ! [])

        ChangeUsername username ->
            { model | loginForm = ( username, second model.loginForm ) } ! []

        ChangePassword password ->
            { model | loginForm = ( first model.loginForm, password ) } ! []


loginForm : ( String, String ) -> Bool -> Html Msg
loginForm ( username, password ) isLoading =
    form [ onSubmit <| Login username password ]
        [ div []
            [ input [ type_ "text", placeholder "username", onInput ChangeUsername ] []
            ]
        , div []
            [ input [ type_ "password", placeholder "password", onInput ChangePassword ] []
            ]
        , if isLoading then
            text "Loading.."
          else
            text ""
        , button [ type_ "submit" ] [ text "Log in" ]
        ]


view : Model -> Html Msg
view model =
    case model.session of
        NotAsked ->
            loginForm model.loginForm False

        Loading ->
            loginForm model.loginForm True

        _ ->
            div [] [ text "loaded" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
