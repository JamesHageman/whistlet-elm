module Types exposing (..)

import RemoteData exposing (..)
import Http


type alias Session =
    { userId : String
    , token : String
    }


type alias Model =
    { session : RemoteData String Session
    , loginForm : ( String, String )
    }


type alias Username =
    String


type alias Password =
    String


type Msg
    = Login Username Password
    | LoginFinish (Result Http.Error Session)
    | ChangeUsername String
    | ChangePassword String
