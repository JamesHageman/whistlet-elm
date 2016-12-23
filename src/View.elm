module View exposing (view)

import Html exposing (Html, Attribute, span, p, text, div, form, input, button, textarea, h1, a)
import Html.Attributes exposing (type_, rows, class, placeholder, value, href, style)
import Html.Events exposing (onSubmit, onInput, onMouseOver, onMouseOut, onClick, onWithOptions, defaultOptions)
import Types exposing (Msg(..), Route(..), Model, broadcastCmp, Broadcast, BroadcastOwner, BroadcastsModel, BroadcastsMsg)
import Data.RemoteData exposing (RemoteData(..))
import Json.Decode
import Http
import Dict
import Navigation exposing (Location)
import Broadcasts.View exposing (broadcastList)


renderBroadcastList : (BroadcastsMsg -> Msg) -> BroadcastsModel -> Model -> Html Msg
renderBroadcastList f broadcasts model =
    broadcastList
        f
        { focusedBroadcast = model.focusedBroadcast
        , session = model.session
        , time = model.time
        , showOwner = ShowOwner
        , fetchOwner = FetchOwner
        , hideOwners = HideOwners
        , link = link
        }
        broadcasts


loginForm : ( String, String ) -> Bool -> Html Msg
loginForm ( username, password ) isLoading =
    form [ onSubmit (Login username password) ]
        [ div []
            [ input
                [ type_ "text"
                , placeholder "username"
                , onInput ChangeUsername
                , value username
                ]
                []
            ]
        , div []
            [ input
                [ type_ "password"
                , placeholder "password"
                , onInput ChangePassword
                , value password
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
        [ composeBox model
        , renderBroadcastList
            HomeBroadcastsMsg
            model.homeBroadcasts
            model
        ]


header : Model -> Html Msg
header model =
    div []
        [ div []
            [ h1 [] [ text "Whistlet" ]
            , link "/" [] [ text "Home" ]
            , text " | "
            , link "/explore" [] [ text "Explore" ]
            , text " | "
            , a [ href "/", onClick Logout ] [ text "Log out" ]
            ]
        , div
            []
            (case model.me of
                Success profile ->
                    [ div [] [ text <| "Signed in as " ++ profile.name ]
                    , div [] [ text <| (toString profile.amp) ++ "dB" ]
                    , div [] [ text <| (toString profile.followersCount) ++ " followers" ]
                    , div [] [ text <| (toString profile.followingCount) ++ " following" ]
                    ]

                Failure err ->
                    [ text <| "Error fetcing profile" ++ (toString err) ]

                Loading ->
                    [ text "..." ]

                NotAsked ->
                    [ text "" ]
            )
        ]


composeBox : Model -> Html Msg
composeBox model =
    form
        [ onSubmit (SendBroadcast model.composeText)
        , class "compose-box"
        ]
        [ div []
            [ textarea
                [ onInput ChangeComposeText
                , value model.composeText
                , class "compose-box__textarea"
                , rows 4
                ]
                []
            ]
        , button [] [ text "Preach it!" ]
        ]


notFoundPage : Location -> Model -> Html Msg
notFoundPage location model =
    div []
        [ h1 [] [ text "404 :(" ]
        , link "/" [] [ text "Go home" ]
        , p [] [ text location.pathname ]
        ]


explorePage : Model -> Html Msg
explorePage model =
    div []
        [ h1 [] [ text "Explore" ]
        , renderBroadcastList
            ExploreBroadcastsMsg
            model.exploreBroadcasts
            model
        ]


profilePage : String -> Model -> Html Msg
profilePage username model =
    case Dict.get username model.profiles of
        Nothing ->
            div [] [ text "initializing..." ]

        Just { profile, broadcasts } ->
            case profile of
                Failure (Http.BadStatus { status }) ->
                    if status.code == 404 then
                        div [] [ text (username ++ " is not a registered user") ]
                    else
                        div [] [ text "whoops! an error occurred" ]

                Failure _ ->
                    div [] [ text "whoops! an error occurred" ]

                Loading ->
                    div [] [ text "..." ]

                NotAsked ->
                    div [] [ text "..." ]

                Success profile ->
                    div []
                        [ h1 [] [ text (profile.name) ]
                        , div []
                            [ text ((toString profile.followersCount) ++ " followers ")
                            , text ((toString profile.followingCount) ++ " following")
                            ]
                          --               , broadcastList model broadcasts
                        ]


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
            div []
                [ header model
                , case model.route of
                    HomePage ->
                        homePage model

                    ExplorePage ->
                        explorePage model

                    ProfilePage name ->
                        profilePage name model

                    NotFoundPage location ->
                        notFoundPage location model
                ]
