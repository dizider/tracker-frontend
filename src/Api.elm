module Api exposing (fetchActiveTrackers, fetchActiveTracks, fetchInitalCoordinates, fetchTrack, fetchTrackers, fetchTracks, trackAssing, trackCreate)

import Decoders as Decoders
import Dict
import Http as Http
import Json.Decode exposing (Decoder, succeed)
import Json.Encode exposing (Value)
import RemoteData exposing (WebData)
import RemoteData.Http as RemoteHttp
import Routing.Helpers as Helpers
import SharedState
import Types as Types
import Url.Builder exposing (QueryParameter, crossOrigin, string)


type alias GetArgs success msg =
    { url : Maybe String
    , decoder : Maybe (Decoder success)
    , tagger : Maybe (WebData success -> msg)
    }


getNewArgs : GetArgs success msg
getNewArgs =
    { url = Nothing
    , decoder = Nothing
    , tagger = Nothing
    }


withOrigin : SharedState.SharedState -> List String -> List QueryParameter -> GetArgs success msg -> GetArgs success msg
withOrigin sharedState pathSegment parameters args =
    { args | url = Just (crossOrigin sharedState.apiOrigin pathSegment parameters) }


withDecoder : Decoder success -> GetArgs success msg -> GetArgs success msg
withDecoder decoder args =
    { args | decoder = Just decoder }


withTagger : (WebData success -> msg) -> GetArgs success msg -> GetArgs success msg
withTagger tagger args =
    { args | tagger = Just tagger }


getJson : GetArgs success msg -> Maybe (Cmd msg)
getJson args =
    Maybe.map3 (\url tagger decoder -> RemoteHttp.getWithConfig RemoteHttp.defaultConfig url tagger decoder) args.url args.tagger args.decoder


getString : GetArgs String msg -> Maybe (Cmd msg)
getString args =
    Maybe.map2
        (\url tagger ->
            Http.get
                { url = url
                , expect = Http.expectString (RemoteData.fromResult >> tagger)
                }
        )
        args.url
        args.tagger


post : String -> (WebData success -> msg) -> Decoder success -> Value -> Cmd msg
post =
    RemoteHttp.postWithConfig RemoteHttp.defaultConfig


fetchTracks : SharedState.SharedState -> (WebData (List Types.Track) -> msg) -> Cmd msg
fetchTracks sharedState tagger =
    getNewArgs
        |> withOrigin sharedState [ "tracks-list" ] []
        |> withDecoder Decoders.decodeTrackList
        |> withTagger tagger
        |> getJson
        |> Maybe.withDefault Cmd.none


fetchActiveTracks : SharedState.SharedState -> (WebData (List Types.Track) -> msg) -> Cmd msg
fetchActiveTracks sharedState tagger =
    getNewArgs
        |> withOrigin sharedState [ "tracks", "active" ] []
        |> withDecoder Decoders.decodeTrackList
        |> withTagger tagger
        |> getJson
        |> Maybe.withDefault Cmd.none


fetchTrackers : SharedState.SharedState -> (WebData (List Types.Tracker) -> msg) -> Cmd msg
fetchTrackers sharedState tagger =
    getNewArgs
        |> withOrigin sharedState [ "trackers-list" ] []
        |> withDecoder Decoders.decodeTrackerList
        |> withTagger tagger
        |> getJson
        |> Maybe.withDefault Cmd.none


fetchActiveTrackers : SharedState.SharedState -> (WebData (List Types.Tracker) -> msg) -> Cmd msg
fetchActiveTrackers sharedState tagger =
    getNewArgs
        |> withOrigin sharedState [ "trackers", "active" ] []
        |> withDecoder Decoders.decodeTrackerList
        |> withTagger tagger
        |> getJson
        |> Maybe.withDefault Cmd.none


fetchInitalCoordinates : SharedState.SharedState -> (WebData (Dict.Dict String Types.Coordinates) -> msg) -> Cmd msg
fetchInitalCoordinates sharedState tagger =
    getNewArgs
        |> withOrigin sharedState [ "tracker", "positions" ] []
        |> withDecoder Decoders.decodeCoordinatesAsDict
        |> withTagger tagger
        |> getJson
        |> Maybe.withDefault Cmd.none


fetchTrack : SharedState.SharedState -> (WebData String -> msg) -> Helpers.TrackId -> Cmd msg
fetchTrack sharedState tagger (Helpers.TrackId id) =
    getNewArgs
        |> withOrigin sharedState [ "list", "gpx", String.fromInt id ] []
        |> withTagger tagger
        |> getString
        |> Maybe.withDefault Cmd.none


trackAssing : SharedState.SharedState -> (WebData String -> msg) -> Helpers.TrackId -> Helpers.TrackerId -> Cmd msg
trackAssing sharedState tagger (Helpers.TrackId track) (Helpers.TrackerId tracker) =
    getNewArgs
        |> withOrigin sharedState [ "track-assign", String.fromInt tracker, String.fromInt track ] []
        |> withTagger tagger
        |> getString
        |> Maybe.withDefault Cmd.none


trackCreate : SharedState.SharedState -> (WebData Types.Tracker -> msg) -> Helpers.TrackerId -> String -> Cmd msg
trackCreate sharedState tagger (Helpers.TrackerId tracker) name =
    getNewArgs
        |> withOrigin sharedState [ "track-create", String.fromInt tracker, name ] [ string "assign" "True" ]
        |> withTagger tagger
        |> withDecoder Decoders.decodeTracker
        |> getJson
        |> Maybe.withDefault Cmd.none
