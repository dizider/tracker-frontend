module Pages.Map exposing (Model, Msg, fetchData, init, update, view)

import Css exposing (..)
import Decoders as Decoders
import Dict exposing (Dict)
import Html
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (attribute, css, id)
import Html.Styled.Events exposing (..)
import Http as Http
import Ports as Ports
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as RHttp
import Routing.Helpers as Helpers
import SharedState as SharedState
import Types exposing (Track)


type alias Model =
    { tracks : List Track
    }


type Msg
    = NoOp
    | AddTrack Int (Result Http.Error String)
    | RemoveTrack Int



-- | ReloadData


init : List Track -> ( Model, Cmd Msg )
init tracks =
    ( { tracks = tracks
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
    let
        _ =
            Debug.log "Fetch data of tacks" tracks
    in
    Cmd.batch (List.map (\track -> fetchTrack track) tracks)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTrack id result ->
            let
                _ =
                    Debug.log "Upadate map" result
            in
            case result of
                Ok trackGpx ->
                    ( model, Ports.addTrack ( id, trackGpx ) )

                Err err ->
                    ( model, Cmd.none )

        RemoveTrack id ->
            ( model, Ports.removeTrack id )

        NoOp ->
            ( model, Cmd.none )


view : SharedState.SharedState -> Model -> Html Msg
view sharedState model =
    div []
        [ h1 [] [ text "Map" ]
        , mapView []
        ]


mapView : List (Attribute msg) -> Html msg
mapView att =
    node "seznam-maps" [ id "maps", attribute "height" "100vh", attribute "width" "100vw" ] [ div [ id "map", css [ height (vh 100), width (vw 100) ] ] [] ]
