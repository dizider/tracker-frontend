module Api exposing (fetchActiveTrackers, fetchActiveTracks, fetchInitalCoordinates, fetchTrack, fetchTrackers, fetchTracks, trackAssing, trackCreate)

import Decoders as Decoders
import Http as Http
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import RemoteData exposing (WebData)
import RemoteData.Http as RemoteHttp
import Routing.Helpers as Helpers
import Types as Types
import Url.Builder exposing (QueryParameter, crossOrigin, string)


origin : String
origin =
    "***REMOVED***"


withOrigin : List String -> List QueryParameter -> String
withOrigin =
    crossOrigin origin


get : String -> (WebData success -> msg) -> Decoder success -> Cmd msg
get =
    RemoteHttp.getWithConfig RemoteHttp.defaultConfig


post : String -> (WebData success -> msg) -> Decoder success -> Value -> Cmd msg
post =
    RemoteHttp.postWithConfig RemoteHttp.defaultConfig


fetchTracks : (WebData success -> msg) -> Decoder success -> Cmd msg
fetchTracks =
    get <| withOrigin [ "tracks-list" ] []


fetchActiveTracks : (WebData success -> msg) -> Decoder success -> Cmd msg
fetchActiveTracks =
    get <| withOrigin [ "tracks", "active" ] []


fetchTrackers : (WebData (List Types.Tracker) -> msg) -> Decoder (List Types.Tracker) -> Cmd msg
fetchTrackers =
    get <| withOrigin [ "trackers-list" ] []


fetchActiveTrackers : (WebData success -> msg) -> Decoder success -> Cmd msg
fetchActiveTrackers =
    get <| withOrigin [ "trackers", "active" ] []


fetchInitalCoordinates : (WebData success -> msg) -> Decoder success -> Cmd msg
fetchInitalCoordinates =
    get <| withOrigin [ "tracker", "positions" ] []


fetchTrack : (WebData String -> msg) -> Helpers.TrackId -> Cmd msg
fetchTrack response (Helpers.TrackId id) =
    Http.get
        { url = withOrigin [ "list", "gpx", String.fromInt id ] []
        , expect = Http.expectString (RemoteData.fromResult >> response)
        }


trackAssing : (Result Http.Error String -> msg) -> Helpers.TrackId -> Helpers.TrackerId -> Cmd msg
trackAssing response (Helpers.TrackId track) (Helpers.TrackerId tracker) =
    Http.get
        { url = withOrigin [ "track-assign", String.fromInt tracker, String.fromInt track ] []
        , expect = Http.expectString response
        }


trackCreate : (Result Http.Error Types.Tracker -> msg) -> Helpers.TrackerId -> String -> Cmd msg
trackCreate response (Helpers.TrackerId tracker) name =
    Http.get
        { url = withOrigin [ "track-create", String.fromInt tracker, name ] [ string "assign" "True" ]
        , expect = Http.expectJson response Decoders.decodeTracker
        }
