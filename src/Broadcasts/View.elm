module Broadcasts.View exposing (Props, broadcastList)

import Html exposing (Html, div, text, button, span)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Types exposing (Broadcast, BroadcastsMsg(..), BroadcastsModel, BroadcastOwner, Session, broadcastCmp)
import Time exposing (Time)
import Data.RemoteData exposing (RemoteData(..))
import Data.RemoteCollection as RemoteCollection
import Http
import Date


type alias Props msg =
    { focusedBroadcast : Maybe Broadcast
    , session : RemoteData Http.Error Session
    , time : Time
    , showOwner : Broadcast -> msg
    , fetchOwner : Broadcast -> msg
    , hideOwners : msg
    }


broadcastList : (BroadcastsMsg -> msg) -> Props msg -> BroadcastsModel -> Html msg
broadcastList f props model =
    div [ class "broadcast-list" ]
        [ RemoteCollection.foldFront
            (text "")
            (text "loading...")
            renderError
            model
        , model
            |> RemoteCollection.items
            |> List.filter (broadcastNotExpired props.time)
            |> List.map (broadcastRow f props)
            |> div []
        , RemoteCollection.foldBack
            (button
                [ onClick
                    (FetchBroadcasts
                        (model
                            |> RemoteCollection.items
                            |> last
                            |> Maybe.map .createdAt
                        )
                    )
                ]
                [ text "load more" ]
                |> Html.map f
            )
            (text "loading...")
            renderError
            model
        ]


broadcastRow : (BroadcastsMsg -> msg) -> Props msg -> Broadcast -> Html msg
broadcastRow f props b =
    let
        cls =
            class "broadcast-row__hover-target"

        showOnHover =
            case b.owner of
                NotAsked ->
                    onMouseOver (props.fetchOwner b)

                _ ->
                    onMouseOver (props.showOwner b)

        hideOnMouseOut =
            onMouseOut props.hideOwners

        attrs =
            [ cls, showOnHover, hideOnMouseOut ]

        broadcastContent =
            text b.text

        opacity =
            props.focusedBroadcast
                |> Maybe.map (always "0")
                |> Maybe.withDefault "1"

        owner =
            case props.focusedBroadcast of
                Just b0 ->
                    if broadcastCmp b0 b then
                        [ renderOwner b.owner (props.fetchOwner b) ]
                    else
                        []

                Nothing ->
                    []
    in
        div [ class "broadcast-row" ]
            [ div attrs
                ([ div
                    [ style [ ( "opacity", opacity ) ]
                    ]
                    [ broadcastContent ]
                 ]
                    ++ owner
                )
            ]


renderOwner : RemoteData Http.Error BroadcastOwner -> msg -> Html msg
renderOwner owner retry =
    div [ class "broadcast-row__owner" ]
        (case owner of
            Loading ->
                [ text "loading" ]

            Success owner ->
                [ text owner.name
                , span [ class "broadcast-row__username" ]
                    [ text ("@" ++ owner.username)
                    ]
                ]

            _ ->
                [ text "whoops! an error occurred..."
                , button [ onClick retry ] [ text "try again" ]
                ]
        )


broadcastNotExpired : Time.Time -> Broadcast -> Bool
broadcastNotExpired time b =
    time - (Date.toTime b.createdAt) < (24 * Time.hour)


renderError : Http.Error -> Html msg
renderError err =
    text ("whoops! an error occured: " ++ (toString err))


last : List a -> Maybe a
last xs =
    xs
        |> List.drop ((List.length xs) - 1)
        |> List.head
