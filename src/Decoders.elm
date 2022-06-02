module Decoders exposing (decodeTrack, decodeTrackList, decodeCoordinates, decodeCoordinatesAsDict)

import Html.Attributes exposing (required)
import Http exposing (request)
import Json.Decode exposing (Decoder, at, dict, keyValuePairs, field, float, int, string, succeed, map)
import Json.Decode.Pipeline exposing (requiredAt)
import Types exposing (Track, Coordinates)
import Dict as Dict
import Routing.Helpers exposing (TrackId)

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