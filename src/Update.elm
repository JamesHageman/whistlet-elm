module Update exposing (update, init)

import Types
    exposing
        ( Model
        , Flags
        , Msg(..)
        , BroadcastsMsg(ReceiveNewBroadcast, ReceiveOwner, LoadOwner, SendNewBroadcast)
        , Session
        , Route(..)
        , Broadcast
        , BroadcastOwner
        , broadcastCmp
        , ProfilePageState
        )
import Data.RemoteCollection as RemoteCollection exposing (RemoteCollection)
import Data.RemoteData as RemoteData exposing (RemoteData(Failure, Success, Loading, NotAsked))
import Navigation exposing (Location, newUrl)
import Tuple exposing (first, second)
import Task
import Time
import Http
import Router
import Api
import Ports
import Return
import Date exposing (Date)
import Dict
import Broadcasts.Model


getInitialModel : Location -> Model
getInitialModel location =
    { session = NotAsked
    , loginForm = ( "", "" )
    , homeBroadcasts = Broadcasts.Model.init
    , exploreBroadcasts = Broadcasts.Model.init
    , focusedBroadcast = Nothing
    , composeText = ""
    , route = Router.parse location
    , time = 0
    , me = NotAsked
    , profiles = Dict.fromList []
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
                        |> Return.command
                            (Broadcasts.Model.load
                                (Api.fetchHomeBroadcasts (Success session) Nothing)
                                |> Cmd.map HomeBroadcastsMsg
                            )
                        |> Return.command
                            (Broadcasts.Model.load
                                (Api.fetchExploreBroadcasts (Success session) Nothing)
                                |> Cmd.map ExploreBroadcastsMsg
                            )
    in
        model
            ! [ fx, Task.perform TimeUpdate Time.now ]
            |> Return.andThen (loadRoute Nothing model.route)


updateBroadcasts : BroadcastsMsg -> Model -> ( Model, Cmd Msg )
updateBroadcasts subMsg model =
    model
        ! []
        |> Return.andThen (updateHomeBroadcasts subMsg)
        |> Return.andThen (updateExploreBroadcasts subMsg)


profilePageLoadingState : ProfilePageState
profilePageLoadingState =
    { broadcasts = Broadcasts.Model.init
    , followers = RemoteCollection.empty
    , following = RemoteCollection.empty
    , profile = Loading
    }


profilePageLoadingFx : Model -> String -> Cmd Msg
profilePageLoadingFx model username =
    Http.send
        (FetchedOtherProfile username)
        (Api.fetchProfileByUsername model.session username)


loadProfile : String -> Model -> ( Model, Cmd Msg )
loadProfile username model =
    let
        ( newProfile, fx ) =
            case Dict.get username model.profiles of
                Nothing ->
                    ( profilePageLoadingState, profilePageLoadingFx model username )

                Just profile ->
                    ( profile, Cmd.none )
    in
        { model
            | profiles = model.profiles |> Dict.insert username newProfile
        }
            ! [ fx ]


loadRoute : Maybe Route -> Route -> Model -> ( Model, Cmd Msg )
loadRoute prevRoute route model =
    if prevRoute == Just route then
        model ! []
    else
        case route of
            ProfilePage username ->
                loadProfile username model

            _ ->
                model ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        UrlChange location ->
            let
                route =
                    Router.parse location
            in
                Return.singleton { model | route = Router.parse location }
                    |> Return.andThen (loadRoute (Just model.route) route)

        Login username password ->
            { model | session = Loading }
                ! [ Http.send LoginFinish
                        (Api.login username password)
                  ]

        LoginFinish (Ok session) ->
            Return.singleton { model | session = Success session }
                |> Return.andThen (update (FetchProfileById session.userId))
                |> Return.command (Ports.saveSession session)

        LoginFinish (Err err) ->
            { model
                | session = Failure err
                , loginForm = ( first model.loginForm, "" )
            }
                ! []

        Logout ->
            model ! [ Ports.logout () ]

        ChangeUsername username ->
            { model | loginForm = ( username, second model.loginForm ) } ! []

        ChangePassword password ->
            { model | loginForm = ( first model.loginForm, password ) } ! []

        ChangeComposeText text ->
            { model | composeText = text } ! []

        SendBroadcast text ->
            model
                ! [ Http.send (HomeBroadcastsMsg << Types.ReceiveNewBroadcast)
                        (Api.sendBroadcast model.session text)
                  ]
                |> Return.andThen (update (HomeBroadcastsMsg SendNewBroadcast))

        Push url ->
            model ! [ newUrl url ]

        FetchOwner broadcast ->
            let
                markBroadcastAsLoading : Model -> ( Model, Cmd Msg )
                markBroadcastAsLoading =
                    updateBroadcasts
                        (LoadOwner broadcast)

                fetchOwnerCmd : Cmd Msg
                fetchOwnerCmd =
                    Http.send
                        (FetchedOwner broadcast)
                        (Api.fetchBroadcastOwner broadcast model.session)
            in
                model
                    |> Return.singleton
                    |> Return.andThen markBroadcastAsLoading
                    |> Return.command fetchOwnerCmd
                    |> Return.andThen (update (ShowOwner broadcast))

        FetchedOwner broadcast res ->
            updateBroadcasts
                (ReceiveOwner broadcast res)
                model

        ShowOwner broadcast ->
            { model | focusedBroadcast = Just broadcast } ! []

        HideOwners ->
            { model | focusedBroadcast = Nothing } ! []

        TimeUpdate time ->
            { model | time = time } ! []

        FetchProfileById id ->
            { model | me = Loading }
                ! [ Http.send FetchedMyProfile (Api.fetchProfileById model.session id) ]

        FetchedMyProfile profile ->
            { model | me = RemoteData.fromResult profile } ! []

        FetchedOtherProfile username profile ->
            { model
                | profiles =
                    model.profiles
                        |> Dict.update username
                            (\state ->
                                case state of
                                    Just state ->
                                        Just { state | profile = RemoteData.fromResult profile }

                                    Nothing ->
                                        Just { profilePageLoadingState | profile = RemoteData.fromResult profile }
                            )
            }
                ! []

        HomeBroadcastsMsg msg ->
            updateHomeBroadcasts msg model
                |> Return.map
                    (case msg of
                        ReceiveNewBroadcast _ ->
                            (\model -> { model | composeText = "" })

                        _ ->
                            identity
                    )

        ExploreBroadcastsMsg msg ->
            updateExploreBroadcasts msg model

        ProfilePageBroadcastsMsg username msg ->
            model ! []


updateHomeBroadcasts : BroadcastsMsg -> Model -> ( Model, Cmd Msg )
updateHomeBroadcasts msg model =
    let
        updateProps =
            { fetchBroadcasts = Api.fetchHomeBroadcasts model.session }

        ( broadcasts, fx ) =
            Broadcasts.Model.update updateProps msg model.homeBroadcasts
    in
        { model | homeBroadcasts = broadcasts } ! [ fx |> Cmd.map HomeBroadcastsMsg ]


updateExploreBroadcasts : BroadcastsMsg -> Model -> ( Model, Cmd Msg )
updateExploreBroadcasts msg model =
    let
        updateProps =
            { fetchBroadcasts = Api.fetchExploreBroadcasts model.session }

        ( broadcasts, fx ) =
            Broadcasts.Model.update updateProps msg model.exploreBroadcasts
    in
        { model | exploreBroadcasts = broadcasts } ! [ fx |> Cmd.map ExploreBroadcastsMsg ]
