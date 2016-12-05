module View exposing (view)

import Html exposing (Html, Attribute, span, p, text, div, form, input, button, textarea, h1, a)
import Html.Attributes exposing (type_, rows, class, placeholder, value, href)
import Html.Events exposing (onSubmit, onInput, onMouseOver, onMouseOut, onClick, onWithOptions, defaultOptions)
import Types exposing (Msg(..), Route(..), Model, broadcastCmp, Broadcast, BroadcastOwner)
import Data.RemoteData exposing (RemoteData(..))
import Data.RemoteCollection as RemoteCollection exposing (RemoteCollection)
import Json.Decode
import Http
import Date
import Time
import Navigation exposing (Location)


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
        , broadcastList model model.homeBroadcasts
        ]


header : Model -> Html Msg
header model =
    div []
        [ h1 [] [ text "Whistlet" ]
        , link "/" [] [ text "home" ]
        , text " | "
        , link "/explore" [] [ text "explore" ]
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


renderError : Http.Error -> Html Msg
renderError err =
    text ("whoops! an error occured: " ++ (toString err))


broadcastRow : Model -> Broadcast -> Html Msg
broadcastRow model b =
    let
        cls =
            class "broadcast-row__hover-target"

        showOnHover =
            case b.owner of
                NotAsked ->
                    onMouseOver (FetchOwner b)

                _ ->
                    onMouseOver (ShowOwner b)

        hideOnMouseOut =
            onMouseOut HideOwners

        attrs =
            [ cls, showOnHover, hideOnMouseOut ]

        children =
            case model.focusedBroadcast of
                Just b0 ->
                    if broadcastCmp b0 b then
                        [ renderOwner b.owner (FetchOwner b) ]
                    else
                        []

                Nothing ->
                    [ text b.text ]
    in
        div [ class "broadcast-row" ]
            [ div attrs children ]


renderOwner : RemoteData Http.Error BroadcastOwner -> Msg -> Html Msg
renderOwner owner retry =
    case owner of
        Loading ->
            div [] [ text "loading" ]

        Success owner ->
            div []
                [ text owner.name
                , span [ class "broadcast-row__username" ]
                    [ text ("@" ++ owner.username)
                    ]
                ]

        _ ->
            div []
                [ text "whoops! an error occurred..."
                , button [ onClick retry ] [ text "try again" ]
                ]


broadcastNotExpired : Time.Time -> Broadcast -> Bool
broadcastNotExpired time b =
    time - (Date.toTime b.createdAt) < (24 * Time.hour)


broadcastList : Model -> RemoteCollection Http.Error Broadcast -> Html Msg
broadcastList model data =
    div [ class "broadcast-list" ]
        [ RemoteCollection.foldFront
            (text "")
            (text "loading...")
            renderError
            data
        , data
            |> RemoteCollection.items
            |> List.filter (broadcastNotExpired model.time)
            |> List.map (broadcastRow model)
            |> div []
        , RemoteCollection.foldBack
            (button
                [ onClick
                    (FetchBroadcasts
                        model.route
                        (data
                            |> RemoteCollection.items
                            |> last
                            |> Maybe.map .createdAt
                        )
                    )
                ]
                [ text "load more" ]
            )
            (text "loading...")
            renderError
            data
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
        , broadcastList model model.exploreBroadcasts
        ]


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
            div []
                [ header model
                , case model.route of
                    Home ->
                        homePage model

                    Explore ->
                        explorePage model

                    Profile name ->
                        profilePage name model

                    NotFound location ->
                        notFoundPage location model
                ]


last : List a -> Maybe a
last xs =
    xs
        |> List.drop ((List.length xs) - 1)
        |> List.head
