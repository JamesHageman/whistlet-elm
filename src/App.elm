module App exposing (..)

import Html exposing (Html, Attribute, text, div, form, input, button, textarea, h1, a)
import Html.Attributes exposing (type_, placeholder, value, href)
import Html.Events exposing (onSubmit, onInput, onClick, onWithOptions, defaultOptions)
import Tuple exposing (first, second)
import RemoteData exposing (RemoteData(Failure, Success, Loading, NotAsked))
import RemoteCollection exposing (remoteCollection, loadFront, loadBack, insertFront, insertBack, errorFront, errorBack)
import Types exposing (Model, Msg(..), Session, Route(..))
import Api
import Http
import Navigation exposing (Location, newUrl)
import Router
import Json.Decode


init : Location -> ( Model, Cmd Msg )
init location =
    ( { session = NotAsked
      , loginForm = ( "", "" )
      , homeBroadcasts = remoteCollection
      , composeText = ""
      , route = Router.parse location
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        UrlChange location ->
            { model | route = Router.parse location } ! []

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
            { model | homeBroadcasts = loadBack model.homeBroadcasts }
                ! [ Http.send FetchedBroadcasts
                        (Api.fetchHomeBroadcasts model)
                  ]

        FetchedBroadcasts (Ok bs) ->
            { model | homeBroadcasts = insertBack bs model.homeBroadcasts } ! []

        FetchedBroadcasts (Err err) ->
            { model | homeBroadcasts = errorBack err model.homeBroadcasts } ! []

        ChangeUsername username ->
            { model | loginForm = ( username, second model.loginForm ) } ! []

        ChangePassword password ->
            { model | loginForm = ( first model.loginForm, password ) } ! []

        ChangeComposeText text ->
            { model | composeText = text } ! []

        SendBroadcast text ->
            { model
                | homeBroadcasts = loadFront model.homeBroadcasts
            }
                ! [ Http.send ReceiveNewBroadcast
                        (Api.sendBroadcast model text)
                  ]

        ReceiveNewBroadcast (Ok b) ->
            { model
                | homeBroadcasts =
                    insertFront [ b ] model.homeBroadcasts
                , composeText = ""
            }
                ! []

        ReceiveNewBroadcast (Err err) ->
            { model
                | homeBroadcasts = errorFront err model.homeBroadcasts
            }
                ! []

        Push url ->
            model ! [ newUrl url ]


loginForm : ( String, String ) -> Bool -> Html Msg
loginForm ( username, password ) isLoading =
    form [ onSubmit <| Login username password ]
        [ div []
            [ input
                [ type_ "text"
                , placeholder "username"
                , onInput ChangeUsername
                ]
                []
            ]
        , div []
            [ input
                [ type_ "password"
                , placeholder "password"
                , onInput ChangePassword
                ]
                []
            ]
        , if isLoading then
            text "Loading.."
          else
            text ""
        , button [ type_ "submit" ] [ text "Log in" ]
        ]


link : String -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
link to attrs children =
    a
        ([ href to
         , onWithOptions
            "click"
            { defaultOptions | preventDefault = True }
            (Json.Decode.succeed (Push to))
         ]
            ++ attrs
        )
        children


homePage : Model -> Html Msg
homePage model =
    div []
        [ header model
        , composeBox model
        , broadcastList model
        ]


header : Model -> Html Msg
header model =
    div []
        [ h1 [] [ text "Whistlet" ]
        , link "/explore" [] [ text "explore" ]
        ]


composeBox : Model -> Html Msg
composeBox model =
    form [ onSubmit <| SendBroadcast model.composeText ]
        [ div []
            [ textarea
                [ onInput ChangeComposeText
                , value model.composeText
                ]
                []
            ]
        , button [] [ text "Preach it!" ]
        ]


broadcastList : Model -> Html Msg
broadcastList model =
    div []
        [ RemoteCollection.foldFront
            (text "")
            (text "loading...")
            (\err -> text <| "whoops! an error occured: " ++ (toString err))
            model.homeBroadcasts
        , div []
            (model.homeBroadcasts
                |> RemoteCollection.items
                |> List.map (\b -> div [] [ text b.text ])
            )
        ]


notFoundPage : Location -> Model -> Html Msg
notFoundPage location model =
    Debug.crash "TODO not found"


explorePage : Model -> Html Msg
explorePage model =
    Debug.crash "TODO explore"


profilePage : String -> Model -> Html Msg
profilePage name model =
    Debug.crash ("TODO profile " ++ name)


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
            case model.route of
                Home ->
                    homePage model

                Explore ->
                    explorePage model

                Profile name ->
                    profilePage name model

                NotFound location ->
                    notFoundPage location model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
