module Broadcasts.View exposing (Props, broadcastList)

import Html exposing (Html, Attribute, div, text, button, span)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Html.Keyed
import Html.Lazy exposing (lazy2)
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
    , link : String -> List (Attribute msg) -> List (Html msg) -> Html msg
    }


broadcastList : (BroadcastsMsg -> msg) -> Props msg -> BroadcastsModel -> Html msg
broadcastList convertMsg props model =
    div [ class "broadcast-list" ]
        [ RemoteCollection.foldFront
            (text "")
            (text "loading...")
            renderError
            model
        , model
            |> RemoteCollection.items
            |> List.filter (broadcastNotExpired props.time)
            |> List.map (broadcastRow convertMsg props)
            |> Html.Keyed.node "div" []
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
                |> Html.map convertMsg
            )
            (text "loading...")
            renderError
            model
        ]


broadcastRow : (BroadcastsMsg -> msg) -> Props msg -> Broadcast -> ( String, Html msg )
broadcastRow convertMsg props broadcast =
    let
        cls =
            class "broadcast-row__hover-target"

        showOnHover =
            case broadcast.owner of
                NotAsked ->
                    onMouseOver (FetchOwner broadcast)

                _ ->
                    onMouseOver (ShowOwner broadcast)

        hideOnMouseOut =
            onMouseOut HideOwners

        attrs : List (Attribute msg)
        attrs =
            [ cls, showOnHover, hideOnMouseOut ]
                |> List.map (Html.Attributes.map convertMsg)

        content =
            lazy2 broadcastContent props.time broadcast
                |> Html.map convertMsg

        opacity =
            props.focusedBroadcast
                |> Maybe.map (always "0")
                |> Maybe.withDefault "1"

        owner =
            case props.focusedBroadcast of
                Just focusedBroadcast ->
                    if broadcastCmp focusedBroadcast broadcast then
                        [ renderOwner
                            convertMsg
                            props
                            broadcast.owner
                            (FetchOwner broadcast)
                        ]
                    else
                        []

                Nothing ->
                    []

        html =
            div [ class "broadcast-row" ]
                [ div attrs
                    ([ div
                        [ style [ ( "opacity", opacity ) ] ]
                        [ content ]
                     ]
                        ++ owner
                    )
                ]

        key =
            (toString broadcast.sourceId) ++ "-" ++ (toString broadcast.rebroadcastId)
    in
        ( key, html )


broadcastContent : Time -> Broadcast -> Html BroadcastsMsg
broadcastContent time broadcast =
    let
        fractionTimeLeft : Float
        fractionTimeLeft =
            1 - ((time - (Date.toTime broadcast.createdAt)) / (24 * Time.hour))

        rightRotate =
            if fractionTimeLeft > 0.5 then
                (fractionTimeLeft - 0.5) * -360
            else
                0

        leftRotate =
            if fractionTimeLeft < 0.5 then
                (fractionTimeLeft) * -360
            else
                -180

        rotate amount =
            "rotate(" ++ (toString amount) ++ "deg)"
    in
        div []
            [ div [ class "broadcast-content-container" ] [ text broadcast.text ]
            , div [ class "broadcast-details-container" ]
                [ div [ class "broadcast-time" ]
                    [ div [ class "pie-container" ]
                        [ div [ class "hold" ]
                            [ div
                                [ class "pie-slice"
                                , style [ ( "transform", rotate rightRotate ) ]
                                ]
                                []
                            ]
                        , div [ class "hold", style [ ( "transform", "rotate(180deg)" ) ] ]
                            [ div
                                [ class "pie-slice"
                                , style [ ( "transform", rotate leftRotate ) ]
                                ]
                                []
                            ]
                        , div [ class "pie-foreground" ]
                            [ div [ class "num-rebroadcasts" ]
                                [ text (toString broadcast.rebroadcastCount)
                                ]
                            ]
                        , div [ class "rotation-container" ]
                            [ div [ class "grey" ] []
                            , div [ class "orange" ] []
                            ]
                        ]
                    ]
                ]
            ]


renderOwner : (BroadcastsMsg -> msg) -> Props msg -> RemoteData Http.Error BroadcastOwner -> BroadcastsMsg -> Html msg
renderOwner convertMsg props owner retry =
    div [ class "broadcast-row__owner" ]
        [ case owner of
            Success owner ->
                div []
                    [ text owner.name
                    , props.link ("/profile/" ++ owner.username)
                        [ class "broadcast-row__username" ]
                        [ text ("@" ++ owner.username)
                        ]
                    ]

            Failure err ->
                div []
                    [ text "whoops! an error occurred..."
                    , button [ onClick retry ] [ text "try again" ]
                    ]
                    |> Html.map convertMsg

            Loading ->
                div [] [ text "loading" ]

            NotAsked ->
                div [] [ text "initializing" ]
        ]


broadcastNotExpired : Time.Time -> Broadcast -> Bool
broadcastNotExpired now broadcast =
    now - (Date.toTime broadcast.createdAt) < (24 * Time.hour)


renderError : Http.Error -> Html msg
renderError err =
    text ("whoops! an error occured: " ++ (toString err))


last : List a -> Maybe a
last list =
    list
        |> List.drop ((List.length list) - 1)
        |> List.head
