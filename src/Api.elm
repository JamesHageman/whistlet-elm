module Api
    exposing
        ( login
        , fetchHomeBroadcasts
        , fetchExploreBroadcasts
        , fetchProfileBroadcasts
        , sendBroadcast
        , fetchBroadcastOwner
        , fetchProfileById
        , fetchProfileByUsername
        )

import Http
import Types exposing (Broadcast, BroadcastOwner, Session, Msg(LoginFinish), Profile)
import Json.Decode exposing (Decoder, nullable, string, int, field, succeed, fail, andThen, list, bool)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as JS
import Date exposing (Date)
import Data.RemoteData exposing (mapSuccess, withDefault, RemoteData(NotAsked))
import QueryString exposing (QueryString)
import Date.Extra exposing (toUtcIsoString)


type alias RemoteSession =
    RemoteData Http.Error Session


baseUrl : String
baseUrl =
    "https://api.whistlet.com/v1"


sessionDecoder : Decoder Session
sessionDecoder =
    decode Session
        |> required "user_id" int
        |> required "token" string
        |> field "auth"


userProfileDecoder : Decoder Profile
userProfileDecoder =
    decode Profile
        |> required "id" int
        |> required "name" string
        |> required "username" string
        |> required "amp" int
        |> required "avatar_url" (nullable string)
        |> required "did_follow" bool
        |> required "follows_you" bool
        |> required "followers" int
        |> required "following" int
        |> required "created_at" stringToDate
        |> field "user"


login : String -> String -> Http.Request Session
login username password =
    let
        body : Http.Body
        body =
            Http.jsonBody <|
                JS.object
                    [ ( "user", JS.string username )
                    , ( "password", JS.string password )
                    ]
    in
        Http.post (baseUrl ++ "/users/login") body sessionDecoder


buildUrl : String -> QueryString.QueryString -> RemoteSession -> String
buildUrl path baseQuery session =
    let
        query =
            session
                |> mapSuccess
                    (\session ->
                        baseQuery |> QueryString.add "token" session.token
                    )
                |> withDefault baseQuery
    in
        baseUrl ++ path ++ (QueryString.render query)


stringToDate : Decoder Date.Date
stringToDate =
    string
        |> andThen
            (\dateString ->
                case Date.fromString dateString of
                    Err err ->
                        fail err

                    Ok date ->
                        succeed date
            )


broadcastOwnerDecoder : Decoder BroadcastOwner
broadcastOwnerDecoder =
    decode BroadcastOwner
        |> required "avatar_url" (nullable string)
        |> required "id" int
        |> required "name" string
        |> required "username" string
        |> required "order_date" (nullable stringToDate)
        |> required "rebroadcast_username" (nullable string)
        |> field "user"


broadcastDecoder : Decoder Broadcast
broadcastDecoder =
    decode Broadcast
        |> required "text" string
        |> required "id" int
        |> required "rebroadcast_count" int
        |> optional "rebroadcast_id" int 0
        |> required "created_at" stringToDate
        |> required "order_date" stringToDate
        |> hardcoded NotAsked


broadcastsDecoder : Decoder (List Broadcast)
broadcastsDecoder =
    list broadcastDecoder
        |> field "broadcasts"


fetchBroadcasts : String -> QueryString -> RemoteSession -> Maybe Date -> Http.Request (List Broadcast)
fetchBroadcasts page baseQuery session orderDate =
    let
        query =
            case orderDate of
                Just date ->
                    baseQuery
                        |> QueryString.add "order_date"
                            (toUtcIsoString date)

                Nothing ->
                    baseQuery
    in
        Http.get (buildUrl ("/broadcasts/" ++ page) query session) broadcastsDecoder


fetchHomeBroadcasts : RemoteSession -> Maybe Date -> Http.Request (List Broadcast)
fetchHomeBroadcasts =
    fetchBroadcasts "home" QueryString.empty


fetchExploreBroadcasts : RemoteSession -> Maybe Date -> Http.Request (List Broadcast)
fetchExploreBroadcasts =
    fetchBroadcasts "explore" QueryString.empty


fetchProfileBroadcasts : Int -> RemoteSession -> Maybe Date -> Http.Request (List Broadcast)
fetchProfileBroadcasts id =
    fetchBroadcasts "profile" (QueryString.empty |> QueryString.add "id" (toString id))


fetchBroadcastOwner : RemoteSession -> Broadcast -> Http.Request BroadcastOwner
fetchBroadcastOwner session broadcast =
    let
        baseQuery =
            QueryString.empty
                |> QueryString.add "broadcast_id" (toString broadcast.sourceId)

        query =
            if broadcast.rebroadcastId == 0 then
                baseQuery
            else
                baseQuery
                    |> QueryString.add "rebroadcast_id" (toString broadcast.rebroadcastId)
    in
        Http.get (buildUrl "/social/broadcast_owner" query session) broadcastOwnerDecoder


sendBroadcast : RemoteSession -> String -> Http.Request Broadcast
sendBroadcast session text =
    let
        body =
            JS.object
                [ ( "text", JS.string text )
                ]
                |> Http.jsonBody
    in
        Http.post (buildUrl "/broadcasts" QueryString.empty session) body (field "broadcast" broadcastDecoder)


fetchProfileById : RemoteSession -> Int -> Http.Request Profile
fetchProfileById session id =
    let
        query =
            QueryString.empty
                |> QueryString.add "id" (toString id)
    in
        Http.get (buildUrl "/users" query session) userProfileDecoder


fetchProfileByUsername : RemoteSession -> String -> Http.Request Profile
fetchProfileByUsername session username =
    let
        query =
            QueryString.empty
                |> QueryString.add "username" username
    in
        Http.get (buildUrl "/users" query session) userProfileDecoder
