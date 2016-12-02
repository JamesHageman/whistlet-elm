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
    , composeText : String
    }


type Msg
    = Login String String
    | LoginFinish (Result Http.Error Session)
    | FetchBroadcasts
    | FetchedBroadcasts (Result Http.Error (List Broadcast))
    | ChangeUsername String
    | ChangePassword String
    | ChangeComposeText String
    | SendBroadcast String
    | ReceiveNewBroadcast (Result Http.Error Broadcast)
