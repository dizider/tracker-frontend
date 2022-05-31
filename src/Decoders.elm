module Decoders exposing (decodeTrack, decodeTrackList)

import Json.Decode exposing (Decoder, at, dict, field, float, int, string, succeed)
import Types exposing (Tracks)
import Http exposing (request)
import Html.Attributes exposing (required)
import Json.Decode.Pipeline exposing (requiredAt)

decodeTrack : Decoder Tracks
decodeTrack = succeed Tracks
    |> requiredAt ["id"] int
    |> requiredAt ["trackerId"] int
    |> requiredAt ["name"] string
    |> requiredAt ["visitedWaypoints"] int

decodeTrackList : Decoder (List Tracks)
decodeTrackList = 
    Json.Decode.list decodeTrack


