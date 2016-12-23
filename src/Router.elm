module Router exposing (parse)

import Types exposing (Route(..))
import UrlParser exposing (Parser, (</>), s, string, oneOf, map, parsePath, top)
import Navigation exposing (Location)


route : Parser (Route -> a) a
route =
    oneOf
        [ map HomePage top
        , map ExplorePage (s "explore")
        , map ProfilePage (s "profile" </> string)
        ]


{-|
  Turn a Location into one of our defined Routes. We only store our current
  route in the model, not the current location. Any url/query parameters in a
  Location should be part of the Route.

    -- in Update.elm
    UrlUpdate location ->
        { model | route = Router.parse location } ! []
-}
parse : Location -> Route
parse location =
    parsePath route location
        |> Maybe.withDefault (NotFoundPage location)
