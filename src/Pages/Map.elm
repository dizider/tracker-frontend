module Pages.Map exposing (init, view, update, Model, Msg)

import Decoders as Decoders
import Dict exposing (Dict)
import Html
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (attribute, id)
import Html.Styled.Events exposing (..)
import Http as Http
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as RHttp
import SharedState as SharedState
import Types exposing (Tracks)
import Ports as Ports

type alias Model =
    { tracks : Dict Int (WebData String)
    }


type Msg
    = NoOp
    | AddTrack Int (Result Http.Error String)
    | RemoveTrack Int
    -- | ReloadData


init : List Tracks -> ( Model, Cmd Msg )
init tracks =
    let
        dictTracks =
            List.map (\track -> ( track.id, Loading )) tracks
    in
    ( { tracks = Dict.fromList dictTracks
      }
    , fetchData tracks
    )


fetchTracks : Int -> Cmd Msg
fetchTracks trackId =
    Http.get
        { url = "https://tracker.dev.jenda.eu/tracks-list"
        , expect = Http.expectString (AddTrack trackId)
        }


fetchData : List Tracks -> Cmd Msg
fetchData tracks =
    Cmd.batch (List.map (\track -> fetchTracks track.id) tracks)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTrack id result ->
            case result of
                Ok trackGpx ->
                    (model, Ports.addTrack (id, trackGpx))
                Err err ->
                    (model, Cmd.none)
        RemoveTrack id ->
            (model, (Ports.removeTrack id))
        
        NoOp ->
            (model, Cmd.none)


view : SharedState.SharedState -> Model -> Html Msg
view sharedState model =
    div []
        [ h1 [] [ text "Map" ]
        , mapView []
        ]


mapView : List (Attribute msg) -> Html msg
mapView att =
    node "seznam-maps" [ id "maps", attribute "height" "100vh", attribute "width" "100vw" ] []
