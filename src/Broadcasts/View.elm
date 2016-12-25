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
                |> Html.map f
            )
            (text "loading...")
            renderError
            model
        ]


broadcastRow : (BroadcastsMsg -> msg) -> Props msg -> Broadcast -> ( String, Html msg )
broadcastRow f props b =
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

        attrs : List (Attribute msg)
        attrs =
            [ cls, showOnHover, hideOnMouseOut ]
                |> List.map (Html.Attributes.map f)

        content =
            lazy2 broadcastContent props.time b
                |> Html.map f

        opacity =
            props.focusedBroadcast
                |> Maybe.map (always "0")
                |> Maybe.withDefault "1"

        owner =
            case props.focusedBroadcast of
                Just b0 ->
                    if broadcastCmp b0 b then
                        [ renderOwner f props b.owner (FetchOwner b) ]
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
            (toString b.sourceId) ++ "-" ++ (toString b.rebroadcastId)
    in
        ( key, html )



-- var rightRotate = props.fractionTimeLeft > 0.5 ? (props.fractionTimeLeft-0.5)*-360 : 0;
-- var leftRotate = props.fractionTimeLeft < 0.5 ? (props.fractionTimeLeft)*-360 : -180;
-- return <div className="pie-container">
--   <div className="hold">
--     <div className="pie-slice"
--           style={{ transform: 'rotate('+ rightRotate +'deg)' }} />
--   </div>
--   <div className="hold" style={{ transform: 'rotate(180deg)' }} >
--     <div className="pie-slice"
--           style={{ transform: 'rotate('+ leftRotate +'deg)' }}/>
--   </div>
--   <div className="pie-foreground" onClick={props.onClick}>
--       <div className="num-rebroadcasts">{props.rebroadcastCount} </div>
--       <div className="rotation-container">
--           <div className="grey"></div>
--           <div className="orange"></div>
--       </div>
--   </div>
-- </div>;


broadcastContent : Time -> Broadcast -> Html BroadcastsMsg
broadcastContent time broadcast =
    let
        fractionTimeLeft : Float
        fractionTimeLeft =
            ((Date.toTime broadcast.createdAt) - time) / (24 * Time.hour)

        rightRotate =
            if fractionTimeLeft > 0.5 then
                (fractionTimeLeft - 0.5) * -360
            else
                0

        leftRotate =
            if fractionTimeLeft < 5 then
                (fractionTimeLeft) * -360
            else
                -180

        rotate amount =
            "rotate(" ++ (toString amount) ++ "deg)"
    in
        div []
            [ span [] [ text broadcast.text ]
            , div [ class "pie-container" ]
                [ div [ class "hold" ]
                    [ div
                        [ class "pie-slice"
                        , style [ ( "transform", rotate rightRotate ) ]
                        ]
                        []
                    ]
                , div [ class "hold" ]
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


renderOwner : (BroadcastsMsg -> msg) -> Props msg -> RemoteData Http.Error BroadcastOwner -> BroadcastsMsg -> Html msg
renderOwner f props owner retry =
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
                    |> Html.map f

            Loading ->
                div [] [ text "loading" ]

            NotAsked ->
                div [] [ text "initializing" ]
        ]


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
