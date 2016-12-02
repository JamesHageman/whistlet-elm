module App exposing (..)

import Html exposing (Html, text, div, form, input, button, textarea, h1)
import Html.Attributes exposing (type_, placeholder, value)
import Html.Events exposing (onSubmit, onInput, onClick)
import Tuple exposing (first, second)
import RemoteData exposing (..)
import Types exposing (Model, Msg(..), Session)
import Api
import Http


init : ( Model, Cmd Msg )
init =
    ( { session = NotAsked
      , loginForm = ( "", "" )
      , homeBroadcasts = NotAsked
      , composeText = ""
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
                { model | session = Failure err } ! []

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
                { model | homeBroadcasts = Failure err } ! []

            ChangeUsername username ->
                { model | loginForm = ( username, second model.loginForm ) } ! []

            ChangePassword password ->
                { model | loginForm = ( first model.loginForm, password ) } ! []

            ChangeComposeText text ->
                { model | composeText = text } ! []

            SendBroadcast text ->
                model ! [ Http.send ReceiveNewBroadcast <| Api.sendBroadcast model text ]

            ReceiveNewBroadcast (Ok b) ->
                { model
                    | homeBroadcasts =
                        case model.homeBroadcasts of
                            Success bs ->
                                Success (b :: bs)

                            _ ->
                                Success [ b ]
                    , composeText = ""
                }
                    ! []

            ReceiveNewBroadcast (Err err) ->
                model ! []


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
    div []
        [ header model
        , composeBox model
        , broadcastList model
        ]


header : Model -> Html Msg
header model =
    h1 [] [ text "Whistlet" ]


composeBox : Model -> Html Msg
composeBox model =
    form [ onSubmit <| SendBroadcast model.composeText ]
        [ div [] [ textarea [ onInput ChangeComposeText, value model.composeText ] [] ]
        , button [] [ text "Preach it!" ]
        ]


broadcastList : Model -> Html Msg
broadcastList model =
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
