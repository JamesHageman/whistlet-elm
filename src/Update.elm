module Update exposing (update, init)

import Types exposing (Model, Flags, Msg(..), Session, Route(..), Broadcast, BroadcastOwner, broadcastCmp)
import Data.RemoteCollection as RemoteCollection exposing (RemoteCollection, remoteCollection, loadFront, loadBack, insertFront, insertBack, errorFront, errorBack)
import Data.RemoteData exposing (RemoteData(Failure, Success, Loading, NotAsked))
import Navigation exposing (Location, newUrl)
import Tuple exposing (first, second)
import Task
import Time
import Http
import Router
import Api
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

        Logout ->
            model ! [ Ports.logout () ]

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
