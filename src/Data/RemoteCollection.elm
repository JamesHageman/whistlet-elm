module Data.RemoteCollection
    exposing
        ( empty
        , RemoteCollection
        , loadFront
        , loadBack
        , errorFront
        , errorBack
        , insertFront
        , insertBack
        , foldFront
        , foldBack
        , items
        , map
        )


type RemoteStatus err
    = NotAsked
    | Loading
    | Failure err


type RemoteCollection err a
    = RemoteCollection
        { data : List a
        , frontStatus : RemoteStatus err
        , backStatus : RemoteStatus err
        }


empty : RemoteCollection x a
empty =
    RemoteCollection
        { data = []
        , frontStatus = NotAsked
        , backStatus = NotAsked
        }


{-| Returns true if no data is in the collection and no data is loading
-}
isEmpty : RemoteCollection x a -> Bool
isEmpty (RemoteCollection col) =
    List.isEmpty col.data
        && (case ( col.frontStatus, col.backStatus ) of
                ( Loading, _ ) ->
                    False

                ( _, Loading ) ->
                    False

                _ ->
                    True
           )


loadFront : RemoteCollection x a -> RemoteCollection x a
loadFront (RemoteCollection col) =
    RemoteCollection { col | frontStatus = Loading }


errorFront : x -> RemoteCollection x a -> RemoteCollection x a
errorFront err (RemoteCollection col) =
    RemoteCollection { col | frontStatus = Failure err }


errorBack : x -> RemoteCollection x a -> RemoteCollection x a
errorBack err (RemoteCollection col) =
    RemoteCollection { col | backStatus = Failure err }


loadBack : RemoteCollection x a -> RemoteCollection x a
loadBack (RemoteCollection col) =
    RemoteCollection { col | backStatus = Loading }


insertFront : List a -> RemoteCollection x a -> RemoteCollection x a
insertFront data (RemoteCollection col) =
    RemoteCollection
        { col
            | frontStatus = NotAsked
            , data = data ++ col.data
        }


insertBack : List a -> RemoteCollection x a -> RemoteCollection x a
insertBack data (RemoteCollection col) =
    RemoteCollection
        { col
            | backStatus = NotAsked
            , data = col.data ++ data
        }


foldFront : a -> a -> (x -> a) -> RemoteCollection x b -> a
foldFront notAsked loading err (RemoteCollection col) =
    case col.frontStatus of
        NotAsked ->
            notAsked

        Loading ->
            loading

        Failure x ->
            err x


foldBack : a -> a -> (x -> a) -> RemoteCollection x b -> a
foldBack notAsked loading err (RemoteCollection col) =
    case col.backStatus of
        NotAsked ->
            notAsked

        Loading ->
            loading

        Failure x ->
            err x


items : RemoteCollection x a -> List a
items (RemoteCollection col) =
    col.data


map : (List a -> List b) -> RemoteCollection x a -> RemoteCollection x b
map f (RemoteCollection col) =
    RemoteCollection
        { col
            | data = f col.data
        }
