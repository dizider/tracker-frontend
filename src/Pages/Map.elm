module Pages.Map exposing (Model, Msg, fetchData, init, update, view)

import Api as Api
import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (id)
import Html.Styled.Events exposing (..)
import Pages.Partials.MapView as MapView
import Ports as Ports
import RemoteData as RD
import Routing.Helpers as Helpers
import SharedState as SharedState
import Types exposing (Track)


type alias Model =
    { tracks : List Track
    , tracksGpx : Dict Int (RD.WebData String)
    , map : MapView.Model Msg
    }


type Msg
    = NoOp
    | AddTrack Helpers.TrackId (RD.WebData String)
    | RemoveTrack Int
    | MapViewMsg MapView.Msg


init : List Track -> ( Model, Cmd Msg )
init tracks =
    ( { tracks = tracks
      , map = MapView.initModel
      , tracksGpx = Dict.empty
      }
    , fetchData (List.map (\t -> Helpers.TrackId t.id) tracks)
    )


fetchData : List Helpers.TrackId -> Cmd Msg
fetchData tracks =
    Cmd.batch
        (List.map
            (\track -> Api.fetchTrack (AddTrack track) track)
            tracks
        )


update : (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update wrapper msg model =
    (\( m, ms ) -> ( m, Cmd.map wrapper ms )) <|
        case msg of
            AddTrack (Helpers.TrackId id) result ->
                case result of
                    RD.Success trackGpx ->
                        ( { model | tracksGpx = Dict.insert id result model.tracksGpx }, Ports.addTrack ( id, trackGpx ) )

                    _ ->
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
view _ model =
    div []
        [ MapView.mapView MapViewMsg model.map []
        ]
