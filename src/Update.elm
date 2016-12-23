module Update exposing (update, init)

import Types
    exposing
        ( Model
        , Flags
        , Msg(..)
        , BroadcastsMsg(ReceiveNewBroadcast, SendNewBroadcast)
        , Session
        , Route(..)
        , Broadcast
        , BroadcastOwner
        , broadcastCmp
        , ProfilePageState
        , Profile
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
    in
        model
            ! [ fx, Task.perform TimeUpdate Time.now ]


loginActions : Session -> Cmd Msg
loginActions session =
    Cmd.batch
        [ Broadcasts.Model.load
            (Api.fetchHomeBroadcasts (Success session) Nothing)
            |> Cmd.map HomeBroadcastsMsg
        , Broadcasts.Model.load
            (Api.fetchExploreBroadcasts (Success session) Nothing)
            |> Cmd.map ExploreBroadcastsMsg
        , Ports.saveSession session
        ]


profilePageEmptyState : Profile -> ProfilePageState
profilePageEmptyState profile =
    Success
        { broadcasts = Broadcasts.Model.init
        , followers = RemoteCollection.empty
        , following = RemoteCollection.empty
        , profile = profile
        }


loadProfilePage : Model -> String -> ProfilePageState -> ( ProfilePageState, Cmd Msg )
loadProfilePage model username profilePage =
    case profilePage of
        Success state ->
            profilePage ! []

        Loading ->
            profilePage ! []

        _ ->
            profilePage
                ! [ Http.send
                        (FetchedOtherProfile username)
                        (Api.fetchProfileByUsername model.session username)
                  ]


loadProfile : String -> Model -> ( Model, Cmd Msg )
loadProfile username model =
    let
        ( newProfile, fx ) =
            model.profiles
                |> Dict.get username
                |> Maybe.withDefault NotAsked
                |> Return.singleton
                |> Return.andThen (loadProfilePage model username)
    in
        { model
            | profiles = model.profiles |> Dict.insert username newProfile
            , focusedBroadcast = Nothing
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
            Return.singleton { model | session = Success session, loginForm = ( "", "" ) }
                |> Return.andThen (update (FetchProfileById session.userId))
                |> Return.command (loginActions session)
                |> Return.andThen (loadRoute Nothing model.route)

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

        TimeUpdate time ->
            { model | time = time } ! []

        FetchProfileById id ->
            { model | me = Loading }
                ! [ Http.send FetchedMyProfile (Api.fetchProfileById model.session id) ]

        FetchedMyProfile profile ->
            { model | me = RemoteData.fromResult profile } ! []

        FetchedOtherProfile username profileResult ->
            let
                newPage : ProfilePageState
                newPage =
                    profileResult
                        |> RemoteData.fromResult
                        |> RemoteData.andThen profilePageEmptyState

                newProfiles =
                    Dict.insert username newPage model.profiles

                fetchBroadcastsCmd =
                    case profileResult of
                        Ok profile ->
                            Broadcasts.Model.load
                                (Api.fetchProfileBroadcasts profile.id model.session Nothing)
                                |> Cmd.map (ProfilePageBroadcastsMsg username)

                        Err _ ->
                            Cmd.none
            in
                { model | profiles = newProfiles } ! [ fetchBroadcastsCmd ]

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
            updateProfilePageBroadcasts username msg model


updateFocusedBroadcast : Maybe (Maybe Broadcast) -> Model -> Model
updateFocusedBroadcast newBroadcast model =
    case newBroadcast of
        Nothing ->
            model

        Just newFocusedBroadcast ->
            { model | focusedBroadcast = newFocusedBroadcast }


updateHomeBroadcasts : BroadcastsMsg -> Model -> ( Model, Cmd Msg )
updateHomeBroadcasts msg model =
    let
        updateProps =
            { fetchBroadcasts = Api.fetchHomeBroadcasts model.session
            , fetchOwner = Api.fetchBroadcastOwner model.session
            }

        ( broadcasts, fx, newFocusedBroadcast ) =
            Broadcasts.Model.update updateProps msg model.homeBroadcasts
    in
        { model | homeBroadcasts = broadcasts }
            ! [ fx |> Cmd.map HomeBroadcastsMsg ]
            |> Return.map (updateFocusedBroadcast newFocusedBroadcast)


updateExploreBroadcasts : BroadcastsMsg -> Model -> ( Model, Cmd Msg )
updateExploreBroadcasts msg model =
    let
        updateProps =
            { fetchBroadcasts = Api.fetchExploreBroadcasts model.session
            , fetchOwner = Api.fetchBroadcastOwner model.session
            }

        ( broadcasts, fx, newFocusedBroadcast ) =
            Broadcasts.Model.update updateProps msg model.exploreBroadcasts
    in
        { model | exploreBroadcasts = broadcasts }
            ! [ fx |> Cmd.map ExploreBroadcastsMsg ]
            |> Return.map (updateFocusedBroadcast newFocusedBroadcast)


updateProfilePageBroadcasts : String -> BroadcastsMsg -> Model -> ( Model, Cmd Msg )
updateProfilePageBroadcasts username msg model =
    case Dict.get username model.profiles of
        Just (Success profilePage) ->
            let
                updateProps =
                    { fetchBroadcasts = Api.fetchProfileBroadcasts profilePage.profile.id model.session
                    , fetchOwner = Api.fetchBroadcastOwner model.session
                    }

                ( broadcasts, fx, newFocusedBroadcast ) =
                    Broadcasts.Model.update updateProps msg profilePage.broadcasts

                newProfilePage : ProfilePageState
                newProfilePage =
                    Success
                        { profilePage | broadcasts = broadcasts }

                wrappedFx =
                    Cmd.map (ProfilePageBroadcastsMsg username) fx

                newModel =
                    { model | profiles = Dict.insert username newProfilePage model.profiles }
            in
                newModel
                    ! [ wrappedFx ]
                    |> Return.map (updateFocusedBroadcast newFocusedBroadcast)

        _ ->
            model ! []
