module Decoders exposing (decodeCoordinates, decodeCoordinatesAsDict, decodeTrack, decodeTrackList, decodeTracker, decodeTrackerList)

import Dict as Dict
import Json.Decode exposing (Decoder, dict, float, int, string, succeed)
import Json.Decode.Pipeline exposing (requiredAt)
import Types exposing (Coordinates, Track, Tracker)


decodeTrack : Decoder Track
decodeTrack =
    succeed Track
        |> requiredAt [ "id" ] int
        |> requiredAt [ "trackerId" ] int
        |> requiredAt [ "name" ] string
        |> requiredAt [ "visitedWaypoints" ] int


decodeTrackList : Decoder (List Track)
decodeTrackList =
    Json.Decode.list decodeTrack


decodeCoordinates : Decoder Coordinates
decodeCoordinates =
    succeed Coordinates
        |> requiredAt [ "id" ] int
        |> requiredAt [ "trackId" ] int
        |> requiredAt [ "time" ] string
        |> requiredAt [ "lat" ] float
        |> requiredAt [ "lon" ] float
        |> requiredAt [ "alt" ] float
        |> requiredAt [ "battery" ] float


decodeCoordinatesAsDict : Decoder (Dict.Dict String Coordinates)
decodeCoordinatesAsDict =
    dict decodeCoordinates


decodeTracker : Decoder Tracker
decodeTracker =
    succeed Tracker
        |> requiredAt [ "id" ] int
        |> requiredAt [ "name" ] string


decodeTrackerList : Decoder (List Tracker)
decodeTrackerList =
    Json.Decode.list decodeTracker
