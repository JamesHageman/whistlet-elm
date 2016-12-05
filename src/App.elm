module App exposing (init, update, view, subscriptions)

import Html exposing (Html, Attribute, span, p, text, div, form, input, button, textarea, h1, a)
import Html.Attributes exposing (type_, rows, class, placeholder, value, href)
import Html.Events exposing (onSubmit, onInput, onMouseOver, onMouseOut, onClick, onWithOptions, defaultOptions)
import Tuple exposing (first, second)
import RemoteData exposing (RemoteData(Failure, Success, Loading, NotAsked))
import RemoteCollection exposing (RemoteCollection, remoteCollection, loadFront, loadBack, insertFront, insertBack, errorFront, errorBack)
import Types exposing (Model, Flags, Msg(..), Session, Route(..), Broadcast, BroadcastOwner)
import Api
import Http
import Navigation exposing (Location, newUrl)
import Router
import Json.Decode
import Time
import Task
import Date
import Ports


getInitialModel : Location -> Model
getInitialModel location =
    { session = NotAsked
    , loginForm = ( "", "" )
    , homeBroadcasts = remoteCollection
    , exploreBroadcasts = remoteCollection
    , focusedBroadcast = Nothing
    , composeText = ""
    , route = Router.parse location
    , time = 0
    }


init : Flags -> Location -> ( Model, Cmd Msg )
init { session } location =
    let
        ( model, fx ) =
            case session of
                Nothing ->
                    getInitialModel location ! []

                Just session ->
                    getInitialModel location
                        |> update (LoginFinish (Ok session))
    in
        model ! [ fx, Task.perform TimeUpdate Time.now ]


updateBroadcasts :
    Route
    -> (RemoteCollection Http.Error Broadcast -> RemoteCollection Http.Error Broadcast)
    -> Model
    -> Model
updateBroadcasts route f model =
    case route of
        Home ->
            { model | homeBroadcasts = f model.homeBroadcasts }

        Explore ->
            { model | exploreBroadcasts = f model.exploreBroadcasts }

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        UrlChange location ->
            { model | route = Router.parse location } ! []

        Login username password ->
            { model | session = Loading }
                ! [ Http.send LoginFinish
                        (Api.login username password)
                  ]

        LoginFinish (Ok session) ->
            let
                new0 =
                    { model | session = Success session }

                ( new1, fx1 ) =
                    update (FetchBroadcasts Home Nothing) new0

                ( new2, fx2 ) =
                    update (FetchBroadcasts Explore Nothing) new1
            in
                new2 ! [ fx1, fx2, Ports.saveSession session ]

        LoginFinish (Err err) ->
            { model
                | session = Failure err
                , loginForm = ( first model.loginForm, "" )
            }
                ! []

        FetchBroadcasts route orderDate ->
            let
                fx =
                    case route of
                        Home ->
                            [ Http.send (FetchedBroadcasts route)
                                (Api.fetchHomeBroadcasts model orderDate)
                            ]

                        Explore ->
                            [ Http.send (FetchedBroadcasts route)
                                (Api.fetchExploreBroadcasts model orderDate)
                            ]

                        _ ->
                            []
            in
                updateBroadcasts route RemoteCollection.loadBack model ! fx

        FetchedBroadcasts route (Ok bs) ->
            updateBroadcasts route (RemoteCollection.insertBack bs) model ! []

        FetchedBroadcasts route (Err err) ->
            updateBroadcasts route (RemoteCollection.errorBack err) model ! []

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

        FetchOwner broadcast ->
            let
                ( new1, fx1 ) =
                    updateBroadcasts
                        model.route
                        (RemoteCollection.map (loadOwner Loading broadcast))
                        model
                        ! [ Http.send
                                (FetchedOwner broadcast)
                                (Api.fetchBroadcastOwner broadcast model)
                          ]

                ( new2, fx2 ) =
                    update (ShowOwner broadcast) new1
            in
                new2 ! [ fx1, fx2 ]

        FetchedOwner broadcast res ->
            let
                owner =
                    case res of
                        Ok x ->
                            Success x

                        Err x ->
                            Failure x
            in
                updateBroadcasts
                    model.route
                    (RemoteCollection.map (loadOwner owner broadcast))
                    model
                    ! []

        ShowOwner broadcast ->
            { model | focusedBroadcast = Just broadcast } ! []

        HideOwners ->
            { model | focusedBroadcast = Nothing } ! []

        TimeUpdate time ->
            { model | time = time } ! []


last : List a -> Maybe a
last xs =
    xs
        |> List.drop ((List.length xs) - 1)
        |> List.head


loadOwner : RemoteData Http.Error BroadcastOwner -> Broadcast -> List Broadcast -> List Broadcast
loadOwner owner b0 bs =
    bs
        |> List.map
            (\b ->
                if broadcastCmp b b0 then
                    { b | owner = owner }
                else
                    b
            )


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


broadcastCmp : Broadcast -> Broadcast -> Bool
broadcastCmp b1 b2 =
    (b1.sourceId == b2.sourceId) && (b1.rebroadcastId == b2.rebroadcastId)


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (30.0 * Time.second) TimeUpdate
