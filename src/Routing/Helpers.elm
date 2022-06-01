module Routing.Helpers exposing (..)


type TrackId
    = TrackId Int


type Route
    = NotFound
    | TracksList
    | HomePage
    | AuthPage
    | MapPage (Maybe TrackId)


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

        MapPage mtrack ->
            case mtrack of
                Nothing ->
                    "/map"

                Just (TrackId id) ->
                    "/map?track=" ++ String.fromInt id
