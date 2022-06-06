module Routing.Helpers exposing (..)

import Url exposing (Url)
import Url.Parser as Parser exposing (..)
import Url.Parser.Query as Query


type TrackId
    = TrackId Int


type Route
    = NotFound
    | AccessDeniedPage
    | TracksList
    | HomePage
    | AuthPage
    | MapPage (Maybe TrackId)


parseUrl : Url -> Route
parseUrl url =
    case Parser.parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


matchRoute : Parser.Parser (Route -> a) a
matchRoute =
    Parser.oneOf
        [ --Parser.map MapPage <| Parser.s "map" </> Parser.map Tracker Parser.int
          Parser.map HomePage (Parser.s "home")
        , Parser.map HomePage Parser.top
        , Parser.map TracksList (Parser.s "trackers")
        , Parser.map AuthPage (Parser.s "auth")
        , Parser.map MapPage <| Parser.s "map" <?> Query.map (Maybe.map TrackId) mapParser
        ]


mapParser : Query.Parser (Maybe Int)
mapParser =
    Query.int "track"


routeToTitle : Route -> String
routeToTitle route =
    case route of
        HomePage ->
            "Home"

        TracksList ->
            "Tracker List"

        NotFound ->
            "NotFound"

        AuthPage ->
            "Autorization"

        AccessDeniedPage ->
            "Access denied"

        MapPage mtrack ->
            case mtrack of
                Nothing ->
                    "Map"

                Just (TrackId track) ->
                    "Track " ++ String.fromInt track


routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            "/not-found"

        TracksList ->
            "/trackers"

        HomePage ->
            "/home"

        AuthPage ->
            "/auth"

        AccessDeniedPage ->
            "/access-denied"

        MapPage mtrack ->
            case mtrack of
                Nothing ->
                    "/map"

                Just (TrackId id) ->
                    "/map?track=" ++ String.fromInt id
