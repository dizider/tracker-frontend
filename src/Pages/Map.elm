module Pages.Map exposing (Model, Msg, fetchData, init, update, view)

import Css exposing (..)
import Decoders as Decoders
import Dict exposing (Dict)
import Html
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (attribute, css, id)
import Html.Styled.Events exposing (..)
import Http as Http
import Pages.Partials.MapView as MapView
import Ports as Ports
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers as Helpers
import SharedState as SharedState
import Types exposing (Track)


type alias Model =
    { tracks : List Track
    , map : MapView.Model
    }


type Msg
    = NoOp
    | AddTrack Int (Result Http.Error String)
    | RemoveTrack Int
    | MapViewMsg MapView.Msg



-- | ReloadData


init : List Track -> ( Model, Cmd Msg )
init tracks =
    ( { tracks = tracks
      , map = MapView.initModel
      }
    , fetchData (List.map (\t -> Helpers.TrackId t.id) tracks)
    )


fetchTrack : Helpers.TrackId -> Cmd Msg
fetchTrack (Helpers.TrackId trackId) =
    Http.get
        { url = "https://tracker.dev.jenda.eu/list/gpx/" ++ String.fromInt trackId
        , expect = Http.expectString (AddTrack trackId)
        }


fetchData : List Helpers.TrackId -> Cmd Msg
fetchData tracks =
    Cmd.batch (List.map (\track -> fetchTrack track) tracks)


update : (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update wrapper msg model =
    (\( m, ms ) -> ( m, Cmd.map wrapper ms )) <|
        case msg of
            AddTrack id result ->
                case result of
                    Ok trackGpx ->
                        ( model, Ports.addTrack ( id, trackGpx ) )

                    Err err ->
                        ( model, Cmd.none )

            RemoveTrack id ->
                ( model, Ports.removeTrack id )

            MapViewMsg mapMsg ->
                let
                    ( mapViewModel, mapViewMsg ) =
                        MapView.update mapMsg model.map
                in
                ( { model | map = mapViewModel }
                , mapViewMsg
                )

            NoOp ->
                ( model, Cmd.none )


view : SharedState.SharedState -> Model -> Html Msg
view sharedState model =
    div []
        [ MapView.mapView MapViewMsg []
        ]
