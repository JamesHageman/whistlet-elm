module Types exposing (..)

import Data.RemoteData exposing (RemoteData)
import Data.RemoteCollection exposing (RemoteCollection)
import Http
import Date exposing (Date)
import Dict exposing (Dict)
import Navigation exposing (Location)
import Time exposing (Time)


type alias BroadcastsModel =
    RemoteCollection Http.Error Broadcast


type BroadcastsMsg
    = FetchBroadcasts (Maybe Date)
    | FetchedBroadcasts (Result Http.Error (List Broadcast))
    | LoadBroadcasts
    | SendNewBroadcast
    | ReceiveNewBroadcast (Result Http.Error Broadcast)
    | FetchOwner Broadcast
    | FetchedOwner Broadcast (Result Http.Error BroadcastOwner)
    | ShowOwner Broadcast
    | HideOwners


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


type alias Profile =
    { id : Int
    , name : String
    , username : String
    , amp : Int
    , avatarUrl : Maybe String
    , didFollow : Bool
    , followsYou : Bool
    , followersCount : Int
    , followingCount : Int
    , createdAt : Date
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


{-| Why is this whole type a RemoteData? Becuase we need the profile to exist
in order to fetch the followers and broadcasts. If ProfilePageState was a
record containing a RemoteData Profile, then there could be a state where the
broadcasts existed, but the profile did not.
-}
type alias ProfilePageState =
    RemoteData Http.Error
        { profile : Profile
        , broadcasts : RemoteCollection Http.Error Broadcast
        , followers : RemoteCollection Http.Error Profile
        , following : RemoteCollection Http.Error Profile
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
    , me : RemoteData Http.Error Profile
    , profiles : Dict String ProfilePageState
    }


type Route
    = HomePage
    | ExplorePage
    | ProfilePage String
    | NotFoundPage Location


type Msg
    = Login String String
    | LoginFinish (Result Http.Error Session)
    | ChangeUsername String
    | ChangePassword String
    | ChangeComposeText String
    | SendBroadcast String
    | UrlChange Location
    | Push String
    | TimeUpdate Time
    | Logout
    | FetchProfileById Int
    | FetchedMyProfile (Result Http.Error Profile)
    | FetchedOtherProfile String (Result Http.Error Profile)
    | HomeBroadcastsMsg BroadcastsMsg
    | ExploreBroadcastsMsg BroadcastsMsg
    | ProfilePageBroadcastsMsg String BroadcastsMsg
