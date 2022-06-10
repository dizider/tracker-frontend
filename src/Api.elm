module Api exposing (fetchActiveTrackers, fetchActiveTracks, fetchInitalCoordinates, fetchTrack, fetchTrackers, fetchTracks)

import Http as Http
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import RemoteData exposing (WebData)
import RemoteData.Http as RemoteHttp
import Routing.Helpers as Helpers
import Types as Types exposing (Track)
import Url.Builder exposing (QueryParameter, absolute, crossOrigin, int)


origin : String
origin =
    "https://tracker.dev.jenda.eu"


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
    get <| withOrigin [ "trackers", "list" ] []


fetchActiveTrackers : (WebData success -> msg) -> Decoder success -> Cmd msg
fetchActiveTrackers =
    get <| withOrigin [ "trackers", "active" ] []


fetchInitalCoordinates : (WebData success -> msg) -> Decoder success -> Cmd msg
fetchInitalCoordinates =
    get <| withOrigin [ "tracker", "positions" ] []


fetchTrack : (WebData String -> msg) -> Helpers.TrackId -> Cmd msg
fetchTrack response (Helpers.TrackId id) =
    Http.get
        { url = withOrigin [ "list", "gpx" ] [ int "id" id ]
        , expect = Http.expectString (RemoteData.fromResult >> response)
        }


trackAssing : (WebData success -> msg) -> Decoder success -> Types.Track -> Types.Tracker -> Cmd msg
trackAssing wrapper decoder track tracker =
    get (withOrigin [ "track-assign" ] [ int "track" track.id, int "tracker" tracker.id ]) wrapper decoder
