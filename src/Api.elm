module Api exposing (login, fetchHomeBroadcasts, fetchExploreBroadcasts, sendBroadcast)

import Task exposing (Task)
import Http
import Types exposing (Model, Broadcast, Session, Msg(LoginFinish))
import Json.Decode exposing (Decoder, string, int, field, succeed, fail, andThen, list)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as JS
import Date
import RemoteData exposing (mapSuccess, withDefault)


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


url path model =
    let
        token =
            model.session
                |> mapSuccess (\session -> session.token)
                |> withDefault ""
    in
        baseUrl ++ path ++ "?token=" ++ token


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


broadcastDecoder : Decoder Broadcast
broadcastDecoder =
    decode Broadcast
        |> required "text" string
        |> required "id" int
        |> required "rebroadcast_count" int
        |> optional "rebroadcast_id" int 0
        |> required "created_at" stringToDate
        |> required "order_date" stringToDate


broadcastsDecoder : Decoder (List Broadcast)
broadcastsDecoder =
    list broadcastDecoder
        |> field "broadcasts"


fetchBroadcasts : String -> Model -> Http.Request (List Broadcast)
fetchBroadcasts page model =
    Http.get (url ("/broadcasts/" ++ page) model) broadcastsDecoder


fetchHomeBroadcasts : Model -> Http.Request (List Broadcast)
fetchHomeBroadcasts =
    fetchBroadcasts "home"


fetchExploreBroadcasts : Model -> Http.Request (List Broadcast)
fetchExploreBroadcasts =
    fetchBroadcasts "explore"


sendBroadcast : Model -> String -> Http.Request Broadcast
sendBroadcast model text =
    let
        body =
            JS.object
                [ ( "text", JS.string text )
                ]
                |> Http.jsonBody
    in
        Http.post (url "/broadcasts" model) body (field "broadcast" broadcastDecoder)
