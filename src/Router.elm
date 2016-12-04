module Router exposing (parse)

import Types exposing (Route(..))
import UrlParser exposing (Parser, (</>), s, string, oneOf, map, parsePath, top)
import Navigation exposing (Location)


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        , map Explore (s "explore")
        , map Profile (s "profile" </> string)
        ]


parse : Location -> Route
parse location =
    parsePath route location
        |> Maybe.withDefault (NotFound location)
