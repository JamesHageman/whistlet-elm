module Update exposing (update, init)

import Types exposing (Model, Flags, Msg(..), Session, Route(..), Broadcast, BroadcastOwner, broadcastCmp)
import Data.RemoteCollection as RemoteCollection exposing (RemoteCollection, remoteCollection, loadFront, loadBack, insertFront, insertBack, errorFront, errorBack)
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
    , me = NotAsked
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
            Return.singleton { model | session = Success session }
                |> Return.andThen (update (FetchBroadcasts Home Nothing))
                |> Return.andThen (update (FetchBroadcasts Explore Nothing))
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

        FetchBroadcasts route orderDate ->
            updateBroadcasts route RemoteCollection.loadBack model
                |> Return.singleton
                |> Return.command
                    (case route of
                        Home ->
                            Http.send (FetchedBroadcasts route)
                                (Api.fetchHomeBroadcasts model orderDate)

                        Explore ->
                            Http.send (FetchedBroadcasts route)
                                (Api.fetchExploreBroadcasts model orderDate)

                        _ ->
                            Cmd.none
                    )

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
                markBroadcastAsLoading : Model -> Model
                markBroadcastAsLoading =
                    updateBroadcasts
                        model.route
                        (RemoteCollection.map (loadOwner Loading broadcast))

                fetchOwnerCmd : Cmd Msg
                fetchOwnerCmd =
                    Http.send
                        (FetchedOwner broadcast)
                        (Api.fetchBroadcastOwner broadcast model)
            in
                model
                    |> markBroadcastAsLoading
                    |> Return.singleton
                    |> Return.command fetchOwnerCmd
                    |> Return.andThen (update (ShowOwner broadcast))

        FetchedOwner broadcast res ->
            updateBroadcasts
                model.route
                (RemoteCollection.map (loadOwner (RemoteData.fromResult res) broadcast))
                model
                ! []

        ShowOwner broadcast ->
            { model | focusedBroadcast = Just broadcast } ! []

        HideOwners ->
            { model | focusedBroadcast = Nothing } ! []

        TimeUpdate time ->
            { model | time = time } ! []

        FetchProfileById id ->
            { model | me = Loading }
                ! [ Http.send FetchedProfile (Api.fetchProfileById model id) ]

        FetchedProfile profile ->
            { model | me = RemoteData.fromResult profile } ! []


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
