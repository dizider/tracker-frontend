module Api exposing (fetchActiveTrackers, fetchActiveTracks, fetchInitalCoordinates, fetchTrack, fetchTrackers, fetchTracks, trackAssing, trackCreate)

import Decoders as Decoders
import Http as Http
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import RemoteData exposing (WebData)
import RemoteData.Http as RemoteHttp
import Routing.Helpers as Helpers
import SharedState
import Types as Types
import Url.Builder exposing (QueryParameter, crossOrigin, string)

withOrigin : SharedState.SharedState -> List String -> List QueryParameter -> String
withOrigin sharedState =
    crossOrigin sharedState.apiOrigin


get : String -> (WebData success -> msg) -> Decoder success -> Cmd msg
get =
    RemoteHttp.getWithConfig RemoteHttp.defaultConfig


post : String -> (WebData success -> msg) -> Decoder success -> Value -> Cmd msg
post =
    RemoteHttp.postWithConfig RemoteHttp.defaultConfig


fetchTracks : SharedState.SharedState -> (WebData success -> msg) -> Decoder success -> Cmd msg
fetchTracks sharedState =
    get <| withOrigin sharedState [ "tracks-list" ] []


fetchActiveTracks : SharedState.SharedState -> (WebData success -> msg) -> Decoder success -> Cmd msg
fetchActiveTracks sharedState =
    get <| withOrigin sharedState [ "tracks", "active" ] []


fetchTrackers : SharedState.SharedState -> (WebData (List Types.Tracker) -> msg) -> Decoder (List Types.Tracker) -> Cmd msg
fetchTrackers sharedState =
    get <| withOrigin sharedState [ "trackers-list" ] []


fetchActiveTrackers : SharedState.SharedState -> (WebData success -> msg) -> Decoder success -> Cmd msg
fetchActiveTrackers sharedState =
    get <| withOrigin sharedState [ "trackers", "active" ] []


fetchInitalCoordinates : SharedState.SharedState -> (WebData success -> msg) -> Decoder success -> Cmd msg
fetchInitalCoordinates sharedState =
    get <| withOrigin sharedState [ "tracker", "positions" ] []


fetchTrack : SharedState.SharedState -> (WebData String -> msg) -> Helpers.TrackId -> Cmd msg
fetchTrack sharedState response (Helpers.TrackId id) =
    Http.get
        { url = withOrigin sharedState [ "list", "gpx", String.fromInt id ] []
        , expect = Http.expectString (RemoteData.fromResult >> response)
        }


trackAssing : SharedState.SharedState -> (Result Http.Error String -> msg) -> Helpers.TrackId -> Helpers.TrackerId -> Cmd msg
trackAssing sharedState response (Helpers.TrackId track) (Helpers.TrackerId tracker) =
    Http.get
        { url = withOrigin sharedState [ "track-assign", String.fromInt tracker, String.fromInt track ] []
        , expect = Http.expectString response
        }


trackCreate : SharedState.SharedState -> (Result Http.Error Types.Tracker -> msg) -> Helpers.TrackerId -> String -> Cmd msg
trackCreate sharedState response (Helpers.TrackerId tracker) name =
    Http.get
        { url = withOrigin sharedState [ "track-create", String.fromInt tracker, name ] [ string "assign" "True" ]
        , expect = Http.expectJson response Decoders.decodeTracker
        }
