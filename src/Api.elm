module Api exposing (login, fetchBroadcasts)

import Task exposing (Task)
import Http
import Types exposing (Model, Broadcast, Session, Msg(LoginFinish))
import Json.Decode exposing (Decoder, string, int, field, succeed, fail, andThen, list)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as JS
import Date
import RemoteData exposing (mapSuccess, withDefault)


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
        Http.post "https://api.whistlet.com/v1/users/login" body sessionDecoder


url path model =
    let
        token =
            model.session
                |> mapSuccess (\session -> session.token)
                |> withDefault ""
    in
        "https://api.whistlet.com/v1" ++ path ++ "?token=" ++ token


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
        |> optional "rebroadcast_id" int 0
        |> required "created_at" stringToDate
        |> required "order_date" stringToDate


broadcastsDecoder : Decoder (List Broadcast)
broadcastsDecoder =
    list broadcastDecoder
        |> field "broadcasts"


fetchBroadcasts : Model -> String -> Http.Request (List Broadcast)
fetchBroadcasts model page =
    Http.get (url "/broadcasts/home" model) broadcastsDecoder
