module Types exposing (..)

import Data.RemoteData exposing (RemoteData)
import Data.RemoteCollection exposing (RemoteCollection)
import Http
import Date exposing (Date)
import Dict exposing (Dict)
import Navigation exposing (Location)
import Time exposing (Time)


type alias BroadcastOwner =
    { avatarUrl : Maybe String
    , id : Int
    , name : String
    , username : String
    , orderDate : Maybe Date
    , rebroadcastUsername : Maybe String
    }


type alias Broadcast =
    { text : String
    , sourceId : Int
    , rebroadcastCount : Int
    , rebroadcastId : Int
    , createdAt : Date
    , orderDate : Date
    , owner : RemoteData Http.Error BroadcastOwner
    }


broadcastCmp : Broadcast -> Broadcast -> Bool
broadcastCmp b1 b2 =
    (b1.sourceId == b2.sourceId) && (b1.rebroadcastId == b2.rebroadcastId)


type alias Session =
    { userId : Int
    , token : String
    }


type alias Flags =
    { session : Maybe Session
    }


type alias Model =
    { session : RemoteData Http.Error Session
    , loginForm : ( String, String )
    , homeBroadcasts : RemoteCollection Http.Error Broadcast
    , exploreBroadcasts : RemoteCollection Http.Error Broadcast
    , focusedBroadcast : Maybe Broadcast
    , composeText : String
    , route : Route
    , time : Time
    }


type Route
    = Home
    | Explore
    | Profile String
    | NotFound Location


type Msg
    = Login String String
    | LoginFinish (Result Http.Error Session)
    | FetchBroadcasts Route (Maybe Date)
    | FetchedBroadcasts Route (Result Http.Error (List Broadcast))
    | FetchOwner Broadcast
    | FetchedOwner Broadcast (Result Http.Error BroadcastOwner)
    | ShowOwner Broadcast
    | HideOwners
    | ChangeUsername String
    | ChangePassword String
    | ChangeComposeText String
    | SendBroadcast String
    | ReceiveNewBroadcast (Result Http.Error Broadcast)
    | UrlChange Location
    | Push String
    | TimeUpdate Time
    | Logout
