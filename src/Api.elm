module Api
    exposing
        ( login
        , fetchHomeBroadcasts
        , fetchExploreBroadcasts
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
import QueryString
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


url : String -> QueryString.QueryString -> RemoteSession -> String
url path qs session =
    let
        query =
            session
                |> mapSuccess
                    (\session ->
                        qs |> QueryString.add "token" session.token
                    )
                |> withDefault qs
    in
        baseUrl ++ path ++ (QueryString.render query)


stringToDate : Decoder Date.Date
stringToDate =
    string
        |> andThen
            (\val ->
                case Date.fromString val of
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


fetchBroadcasts : String -> RemoteSession -> Maybe Date -> Http.Request (List Broadcast)
fetchBroadcasts page session orderDate =
    let
        query =
            case orderDate of
                Just date ->
                    QueryString.empty
                        |> QueryString.add "order_date"
                            (toUtcIsoString date)

                Nothing ->
                    QueryString.empty
    in
        Http.get (url ("/broadcasts/" ++ page) query session) broadcastsDecoder


fetchHomeBroadcasts : RemoteSession -> Maybe Date -> Http.Request (List Broadcast)
fetchHomeBroadcasts =
    fetchBroadcasts "home"


fetchExploreBroadcasts : RemoteSession -> Maybe Date -> Http.Request (List Broadcast)
fetchExploreBroadcasts =
    fetchBroadcasts "explore"


fetchBroadcastOwner : Broadcast -> RemoteSession -> Http.Request BroadcastOwner
fetchBroadcastOwner b session =
    let
        query0 =
            QueryString.empty
                |> QueryString.add "broadcast_id" (toString b.sourceId)

        query =
            if b.rebroadcastId == 0 then
                query0
            else
                query0
                    |> QueryString.add "rebroadcast_id" (toString b.rebroadcastId)
    in
        Http.get (url "/social/broadcast_owner" query session) broadcastOwnerDecoder


sendBroadcast : RemoteSession -> String -> Http.Request Broadcast
sendBroadcast session text =
    let
        body =
            JS.object
                [ ( "text", JS.string text )
                ]
                |> Http.jsonBody
    in
        Http.post (url "/broadcasts" QueryString.empty session) body (field "broadcast" broadcastDecoder)


fetchProfileById : RemoteSession -> Int -> Http.Request Profile
fetchProfileById session id =
    let
        qs =
            QueryString.empty
                |> QueryString.add "id" (toString id)
    in
        Http.get (url "/users" qs session) userProfileDecoder


fetchProfileByUsername : RemoteSession -> String -> Http.Request Profile
fetchProfileByUsername session username =
    let
        qs =
            QueryString.empty
                |> QueryString.add "username" username
    in
        Http.get (url "/users" qs session) userProfileDecoder
