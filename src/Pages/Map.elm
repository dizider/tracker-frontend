module Pages.Map exposing (Model, Msg, fetchData, init, update, view)

import Api as Api
import Bootstrap.Alert as Alert
import Css exposing (..)
import Dict exposing (Dict)
import Html.Attributes
import Html.Styled as SHtml
import Html.Styled.Attributes exposing (id)
import Pages.Partials.LoadingView as Loading
import Pages.Partials.MapView as MapView
import Ports as Ports
import RemoteData as RD
import Routing.Helpers as Helpers
import SharedState
import Types exposing (Track)


type alias Model =
    { tracks : List Track
    , tracksGpx : Dict Int (RD.WebData String)
    , map : MapView.Model Msg
    , isLoading : Bool
    }


type Msg
    = NoOp
    | AddTrack Helpers.TrackId (RD.WebData String)
    | RemoveTrack Int
    | RemoveAll
    | MapViewMsg MapView.Msg


init : Model
init =
    { tracks = []
    , map = MapView.initModel
    , tracksGpx = Dict.empty
    , isLoading = False
    }


fetchData : List Helpers.TrackId -> Model -> SharedState.SharedState -> ( Model, Cmd Msg )
fetchData tracks model sharedState =
    ( { model | tracksGpx = Dict.fromList (List.map (\(Helpers.TrackId id) -> ( id, RD.Loading )) tracks), isLoading = True }
    , Cmd.batch <|
        List.append
            (List.map (\track -> Api.fetchTrack sharedState (AddTrack track) track) tracks)
            (List.map (\id -> Ports.removeTrack id) (Dict.keys model.tracksGpx))
    )


update : (Msg -> msg) -> Msg -> Model -> SharedState.SharedState -> ( Model, Cmd msg, SharedState.SharedStateUpdate )
update wrapper msg model sharedState =
    (\( m, ms, s ) -> ( m, Cmd.map wrapper ms, s )) <|
        case msg of
            AddTrack (Helpers.TrackId id) result ->
                case result of
                    RD.Success trackGpx ->
                        let
                            updatedTracksGpx =
                                Dict.insert id result model.tracksGpx
                        in
                        ( { model | tracksGpx = updatedTracksGpx, isLoading = loadingStatus updatedTracksGpx }, Ports.addTrack ( id, trackGpx ), SharedState.NoUpdate )

                    RD.Failure _ ->
                        let
                            updatedTracksGpx =
                                Dict.remove id model.tracksGpx
                        in
                        ( { model | tracksGpx = updatedTracksGpx, isLoading = loadingStatus updatedTracksGpx }, Cmd.none, SharedState.NoUpdate )

                    _ ->
                        ( model, Cmd.none, SharedState.NoUpdate )

            RemoveTrack id ->
                ( { model | tracksGpx = Dict.remove id model.tracksGpx }, Ports.removeTrack id, SharedState.NoUpdate )

            RemoveAll ->
                ( { model | tracksGpx = Dict.empty }, Cmd.batch (List.map (\id -> Ports.removeTrack id) (Dict.keys model.tracksGpx)), SharedState.NoUpdate )

            MapViewMsg mapMsg ->
                let
                    ( mapViewModel, mapViewMsg, sharedStateUpdate ) =
                        MapView.update mapMsg model.map sharedState
                in
                ( { model | map = mapViewModel }
                , mapViewMsg
                , sharedStateUpdate
                )

            NoOp ->
                ( model, Cmd.none, SharedState.NoUpdate )


loadingStatus : Dict Int (RD.WebData String) -> Bool
loadingStatus model =
    case RD.fromList (Dict.values model) of
        RD.Success _ ->
            False

        RD.Loading ->
            True

        _ ->
            False


view : SharedState.SharedState -> Model -> SHtml.Html Msg
view _ model =
    let
        warning =
            if Dict.isEmpty model.tracksGpx then
                [ Alert.simpleWarning
                    [ Html.Attributes.style "zIndex" "2" ]
                    [ SHtml.text "No track selected" |> SHtml.toUnstyled ]
                    |> SHtml.fromUnstyled
                ]

            else
                []
    in
    if model.isLoading then
        Loading.view |> SHtml.fromUnstyled

    else
        SHtml.div []
            [ MapView.mapView MapViewMsg
                model.map
                warning
            ]
