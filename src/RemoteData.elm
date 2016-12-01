module RemoteData exposing (RemoteData(..), mapSuccess, toMaybe, withDefault)


type RemoteData err data
    = NotAsked
    | Loading
    | Failure err
    | Success data


mapSuccess : (a -> b) -> RemoteData err a -> RemoteData err b
mapSuccess f rd =
    case rd of
        Success data ->
            Success <| f data

        Loading ->
            Loading

        NotAsked ->
            NotAsked

        Failure err ->
            Failure err


toMaybe : RemoteData err a -> Maybe a
toMaybe rd =
    case rd of
        Success data ->
            Just data

        _ ->
            Nothing


withDefault : a -> RemoteData err a -> a
withDefault a rd =
    case rd of
        Success x ->
            x

        _ ->
            a
