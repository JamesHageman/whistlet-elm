module App exposing (..)

import Html exposing (Html, text, div, form, input, button)
import Html.Attributes exposing (type_, placeholder)
import Html.Events exposing (onSubmit, onInput, onClick)
import Tuple exposing (first, second)
import RemoteData exposing (..)
import Types exposing (Model, Msg(..), Username, Password, Session)
import Api
import Http


init : ( Model, Cmd Msg )
init =
    ( { session = NotAsked
      , loginForm = ( "", "" )
      , homeBroadcasts = NotAsked
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
        case msg of
            Login username password ->
                { model | session = Loading }
                    ! [ Http.send LoginFinish <| Api.login username password
                      ]

            LoginFinish (Ok session) ->
                let
                    newModel =
                        { model | session = Success session }
                in
                    update FetchBroadcasts newModel

            LoginFinish (Err err) ->
                model ! []

            FetchBroadcasts ->
                let
                    newBroacasts =
                        case model.homeBroadcasts of
                            NotAsked ->
                                Loading

                            Failure err ->
                                Loading

                            _ ->
                                model.homeBroadcasts
                in
                    { model | homeBroadcasts = newBroacasts }
                        ! [ Http.send FetchedBroadcasts <| Api.fetchBroadcasts model "home"
                          ]

            FetchedBroadcasts (Ok bs) ->
                { model | homeBroadcasts = Success bs } ! []

            FetchedBroadcasts (Err err) ->
                model ! []

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


homePage : Model -> Html Msg
homePage model =
    case model.homeBroadcasts of
        Loading ->
            div [] [ text "..." ]

        NotAsked ->
            div [] [ text "..." ]

        Failure err ->
            div [] [ text "whoops!", button [ onClick FetchBroadcasts ] [ text "try again" ] ]

        Success broadcasts ->
            let
                elems =
                    broadcasts
                        |> List.map (\b -> div [] [ text b.text ])
            in
                div [] elems


view : Model -> Html Msg
view model =
    case model.session of
        NotAsked ->
            loginForm model.loginForm False

        Loading ->
            loginForm model.loginForm True

        Failure err ->
            loginForm model.loginForm False

        Success _ ->
            homePage model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
