module Api
    exposing
        ( login
        , fetchHomeBroadcasts
        , fetchExploreBroadcasts
        , sendBroadcast
        , fetchBroadcastOwner
        , fetchProfileById
        )

import Http
import Types exposing (Model, Broadcast, BroadcastOwner, Session, Msg(LoginFinish), UserProfile)
import Json.Decode exposing (Decoder, nullable, string, int, field, succeed, fail, andThen, list, bool)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as JS
import Date exposing (Date)
import Data.RemoteData exposing (mapSuccess, withDefault, RemoteData(NotAsked))
import QueryString
import Date.Extra exposing (toUtcIsoString)


baseUrl : String
baseUrl =
    "https://api.whistlet.com/v1"


sessionDecoder : Decoder Session
sessionDecoder =
    decode Session
        |> required "user_id" int
        |> required "token" string
        |> field "auth"


userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    decode UserProfile
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


url : String -> QueryString.QueryString -> Model -> String
url path qs model =
    let
        query =
            model.session
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


fetchBroadcasts : String -> Model -> Maybe Date -> Http.Request (List Broadcast)
fetchBroadcasts page model orderDate =
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
        Http.get (url ("/broadcasts/" ++ page) query model) broadcastsDecoder


fetchHomeBroadcasts : Model -> Maybe Date -> Http.Request (List Broadcast)
fetchHomeBroadcasts =
    fetchBroadcasts "home"


fetchExploreBroadcasts : Model -> Maybe Date -> Http.Request (List Broadcast)
fetchExploreBroadcasts =
    fetchBroadcasts "explore"


fetchBroadcastOwner : Broadcast -> Model -> Http.Request BroadcastOwner
fetchBroadcastOwner b model =
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
        Http.get (url "/social/broadcast_owner" query model) broadcastOwnerDecoder


sendBroadcast : Model -> String -> Http.Request Broadcast
sendBroadcast model text =
    let
        body =
            JS.object
                [ ( "text", JS.string text )
                ]
                |> Http.jsonBody
    in
        Http.post (url "/broadcasts" QueryString.empty model) body (field "broadcast" broadcastDecoder)


fetchProfileById : Model -> Int -> Http.Request UserProfile
fetchProfileById model id =
    let
        qs =
            QueryString.empty
                |> QueryString.add "id" (toString id)
    in
        Http.get (url "/users" qs model) userProfileDecoder
