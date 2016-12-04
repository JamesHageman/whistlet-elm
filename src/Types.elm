module Types exposing (..)

import RemoteData exposing (RemoteData)
import RemoteCollection exposing (RemoteCollection)
import Http
import Date exposing (Date)
import Dict exposing (Dict)
import Navigation exposing (Location)


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


type alias Session =
    { userId : Int
    , token : String
    }


type alias Model =
    { session : RemoteData Http.Error Session
    , loginForm : ( String, String )
    , homeBroadcasts : RemoteCollection Http.Error Broadcast
    , exploreBroadcasts : RemoteCollection Http.Error Broadcast
    , focusedBroadcast : Maybe Broadcast
    , composeText : String
    , route : Route
    }


type Route
    = Home
    | Explore
    | Profile String
    | NotFound Location


type Msg
    = Login String String
    | LoginFinish (Result Http.Error Session)
    | FetchBroadcasts Route
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
