module Data.RemoteData
    exposing
        ( RemoteData(..)
        , mapSuccess
        , toMaybe
        , withDefault
        , fromResult
        , andThen
        )


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


fromResult : Result x a -> RemoteData x a
fromResult x =
    case x of
        Ok data ->
            Success data

        Err err ->
            Failure err


andThen : (a -> RemoteData x b) -> RemoteData x a -> RemoteData x b
andThen f data =
    case data of
        Success a ->
            f a

        Failure err ->
            Failure err

        Loading ->
            Loading

        NotAsked ->
            NotAsked
