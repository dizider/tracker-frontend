module Decoders exposing (decodeTracker, decodeTrackerList)

import Json.Decode exposing (Decoder, at, dict, field, float, int, string, succeed)
import Types exposing (Tracker)
import Http exposing (request)
import Html.Attributes exposing (required)
import Json.Decode.Pipeline exposing (requiredAt)

decodeTracker : Decoder Tracker
decodeTracker = succeed Tracker
    |> requiredAt ["tracker_id"] int

decodeTrackerList : Decoder (List Tracker)
decodeTrackerList = 
    Json.Decode.list decodeTracker


