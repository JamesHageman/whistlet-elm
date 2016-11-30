module Api exposing (login)

import Task exposing (Task)
import Http
import Types exposing (Session, Msg(LoginFinish))
import Json.Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, required)


sessionDecoder : Decoder Session
sessionDecoder =
    decode Session
        |> required "user_id" string
        |> required "token" string


login : String -> String -> Http.Request Session
login username password =
    let
        body : Http.Body
        body =
            Http.multipartBody
                [ Http.stringPart "user" username
                , Http.stringPart "password" password
                ]
    in
        Http.post "https://api.whistlet.com/v1/users/login" body sessionDecoder
