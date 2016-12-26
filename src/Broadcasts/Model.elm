module Broadcasts.Model exposing (init, load, update)

import Http
import Data.RemoteCollection as RemoteCollection exposing (RemoteCollection)
import Data.RemoteData exposing (RemoteData(..), fromResult)
import Types exposing (Broadcast, BroadcastOwner, BroadcastsModel, BroadcastsMsg(..), broadcastCmp)
import Date exposing (Date)
import Task
import Return


type alias UpdateProps =
    { fetchBroadcasts : Maybe Date -> Http.Request (List Broadcast)
    , fetchOwner : Broadcast -> Http.Request BroadcastOwner
    }


init : BroadcastsModel
init =
    RemoteCollection.empty


{-| used to load broadcasts into the list from the outside. Note that it returns
a `Cmd BroadcastMsg`, so you must turn this into the appropriate top-level `Msg`
for it to work properly.

    model ! [
        Broadcast.Model.load (Api.fetchHomeBroadcasts model.session)
            |> Cmd.map HomeBroadcastsMsg
    ]
-}
load : Http.Request (List Broadcast) -> Cmd BroadcastsMsg
load req =
    Cmd.batch
        [ Task.perform identity (Task.succeed (LoadBroadcasts))
        , Http.send
            (FetchedBroadcasts)
            req
        ]


update : UpdateProps -> BroadcastsMsg -> BroadcastsModel -> ( BroadcastsModel, Cmd BroadcastsMsg, Maybe (Maybe Broadcast) )
update props msg model =
    let
        ( newModel, fx ) =
            updateModel props msg model

        output =
            case msg of
                FetchOwner broadcast ->
                    Just (Just broadcast)

                ShowOwner broadcast ->
                    Just (Just broadcast)

                HideOwners ->
                    Just Nothing

                _ ->
                    Nothing
    in
        ( newModel, fx, output )


updateModel : UpdateProps -> BroadcastsMsg -> BroadcastsModel -> ( BroadcastsModel, Cmd BroadcastsMsg )
updateModel props msg model =
    case msg of
        FetchBroadcasts orderDate ->
            model
                ! [ Http.send FetchedBroadcasts (props.fetchBroadcasts orderDate)
                  ]
                |> Return.andThen (updateModel props LoadBroadcasts)

        LoadBroadcasts ->
            RemoteCollection.loadBack model ! []

        FetchedBroadcasts (Ok bs) ->
            RemoteCollection.insertBack bs model ! []

        FetchedBroadcasts (Err err) ->
            RemoteCollection.errorBack err model ! []

        SendNewBroadcast ->
            RemoteCollection.loadFront model ! []

        ReceiveNewBroadcast (Ok b) ->
            RemoteCollection.insertFront [ b ] model ! []

        ReceiveNewBroadcast (Err err) ->
            RemoteCollection.errorBack err model ! []

        FetchOwner broadcast ->
            RemoteCollection.map (loadOwner Loading broadcast) model
                ! [ Http.send (FetchedOwner broadcast) (props.fetchOwner broadcast)
                  ]

        FetchedOwner broadcast ownerResult ->
            RemoteCollection.map (loadOwner (fromResult ownerResult) broadcast) model ! []

        ShowOwner broadcast ->
            model ! []

        HideOwners ->
            model ! []


loadOwner : RemoteData Http.Error BroadcastOwner -> Broadcast -> List Broadcast -> List Broadcast
loadOwner owner b0 broadcasts =
    List.map
        (\b ->
            if broadcastCmp b b0 then
                { b | owner = owner }
            else
                b
        )
        broadcasts
