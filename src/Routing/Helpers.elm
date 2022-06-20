module Routing.Helpers exposing (..)

import Json.Decode as Decode exposing (decodeString, list)
import Json.Encode as Encode
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing (..)
import Url.Parser.Query as Query


type TrackId
    = TrackId Int


type TrackerId
    = TrackerId Int


type Route
    = NotFound
    | AccessDeniedPage
    | Tracks
    | Trackers
    | HomePage
    | AuthPage
    | MapPage (Maybe (List TrackId))


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
        , Parser.map Tracks (Parser.s "tracks")
        , Parser.map Trackers (Parser.s "trackers")
        , Parser.map AuthPage (Parser.s "auth")
        , Parser.map MapPage <|
            Parser.s "map"
                <?> Query.map
                        (\ms ->
                            Maybe.andThen
                                (\s -> decodeString (list Decode.int) s |> Result.toMaybe |> Maybe.map (List.map TrackId))
                                ms
                        )
                        mapParser
        ]


mapParser : Query.Parser (Maybe String)
mapParser =
    Query.string "track"


routeToTitle : Route -> String
routeToTitle route =
    case route of
        HomePage ->
            "Home"

        Tracks ->
            "Tracks"

        Trackers ->
            "Trackers"

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

                Just track ->
                    "Track "


routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            "/not-found"

        Tracks ->
            "/tracks"

        Trackers ->
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

                Just track ->
                    let
                        rawTrack =
                            List.map (\(TrackId id) -> id) track
                    in
                    Builder.absolute [ "map" ] [ Builder.string "track" (Encode.encode 0 (Encode.list Encode.int rawTrack)) ]
