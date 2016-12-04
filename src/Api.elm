module Api exposing (login, fetchHomeBroadcasts, fetchExploreBroadcasts, sendBroadcast, fetchBroadcastOwner)

import Http
import Types exposing (Model, Broadcast, BroadcastOwner, Session, Msg(LoginFinish))
import Json.Decode exposing (Decoder, nullable, string, int, field, succeed, fail, andThen, list)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as JS
import Date
import RemoteData exposing (mapSuccess, withDefault, RemoteData(NotAsked))
import QueryString


baseUrl : String
baseUrl =
    "https://api.whistlet.com/v1"


sessionDecoder : Decoder Session
sessionDecoder =
    let
        decoder =
            decode Session
                |> required "user_id" int
                |> required "token" string
    in
        field "auth" decoder


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


fetchBroadcasts : String -> Model -> Http.Request (List Broadcast)
fetchBroadcasts page model =
    Http.get (url ("/broadcasts/" ++ page) QueryString.empty model) broadcastsDecoder


fetchHomeBroadcasts : Model -> Http.Request (List Broadcast)
fetchHomeBroadcasts =
    fetchBroadcasts "home"


fetchExploreBroadcasts : Model -> Http.Request (List Broadcast)
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
