module Types exposing (..)

import RemoteData exposing (..)
import Http
import Date exposing (Date)
import Dict exposing (Dict)


type alias Broadcast =
    { text : String
    , sourceId : Int
    , rebroadcastId : Int
    , createdAt : Date
    , orderDate : Date
    }


type alias Session =
    { userId : Int
    , token : String
    }


type alias Model =
    { session : RemoteData Http.Error Session
    , loginForm : ( String, String )
    , homeBroadcasts : RemoteData Http.Error (List Broadcast)
    }


type alias Username =
    String


type alias Password =
    String


type Msg
    = Login Username Password
    | LoginFinish (Result Http.Error Session)
    | FetchBroadcasts
    | FetchedBroadcasts (Result Http.Error (List Broadcast))
    | ChangeUsername String
    | ChangePassword String
