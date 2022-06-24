module Pages.LiveView exposing (Model, Msg(..), initModel, load, update, view)

import Api
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Browser.Navigation exposing (pushUrl)
import Css exposing (..)
import Dict as Dict
import Html as Html
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Styled as SHtml exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (..)
import Icons
import Pages.Partials.MapView as MapView
import Pages.Partials.TrackSelection as TrackSelection
import Ports as Ports
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers as Helpers exposing (Route(..), routeToString)
import SharedState as SharedState
import Types as Types exposing (Coordinates)


type alias Tracks =
    Dict.Dict String (List Coordinates)


type alias Model =
    { map : MapView.Model Msg
    , trackSelectionVisibility : Modal.Visibility
    , trackSelection : TrackSelection.Model
    , tracks : Tracks
    }


type Msg
    = NoOp
    | NewCoordinates Types.Coordinates
    | InitialCoordinates (WebData Types.Coordinates)
    | NavigateTo Helpers.Route
    | MapViewMsg MapView.Msg
    | TrackSelectionMsg TrackSelection.Msg
    | ShowTracksModal
    | HideTracksModal


initModel : Model
initModel =
    { map = MapView.initModel
    , trackSelectionVisibility = Modal.hidden
    , trackSelection = TrackSelection.initModel
    , tracks = Dict.empty
    }


load : Cmd msg
load =
    MapView.load


mapViewWithButtons : MapView.Model Msg -> MapView.Model Msg
mapViewWithButtons model =
    MapView.addButton trackSelection model


fetchInitalCoordinates : Types.Track -> SharedState.SharedState -> Cmd Msg
fetchInitalCoordinates track sharedState =
    Api.fetchInitalCoordinates
        track
        sharedState
        InitialCoordinates


trackSelection : List (Attribute Msg) -> Html Msg
trackSelection attrs =
    button
        (List.append
            [ onClick ShowTracksModal
            , css
                [ top (px 165)
                , right (px 17)
                ]
            ]
            attrs
        )
        [ Icons.eye |> fromUnstyled, text " Tracks" ]


update : (Msg -> msg) -> SharedState.SharedState -> Msg -> Model -> ( Model, Cmd msg, SharedState.SharedStateUpdate )
update wrapper sharedState msg model =
    (\( m, ms, s ) -> ( m, Cmd.map wrapper ms, s )) <|
        case msg of
            NewCoordinates coords ->
                let
                    updatedTracks =
                        insertCoordinatesToTrack model coords
                in
                ( { model | tracks = updatedTracks }
                , sendTracksToMap updatedTracks
                , SharedState.NoUpdate
                )

            InitialCoordinates webCoord ->
                case webCoord of
                    Success coords ->
                        let
                            updatedTracks =
                                insertCoordinatesToTrack model coords
                        in
                        ( { model | tracks = updatedTracks }
                        , sendTracksToMap updatedTracks
                        , SharedState.NoUpdate
                        )

                    _ ->
                        ( model, Cmd.none, SharedState.NoUpdate )

            NavigateTo route ->
                ( model
                , pushUrl sharedState.navKey (routeToString route)
                , SharedState.NoUpdate
                )

            MapViewMsg mapMsg ->
                let
                    ( mapViewModel, mapViewMsg, sharedStateUpdate ) =
                        MapView.update mapMsg model.map sharedState
                in
                ( { model | map = mapViewModel }
                , mapViewMsg
                , sharedStateUpdate
                )

            ShowTracksModal ->
                ( { model | trackSelectionVisibility = Modal.shown }
                , Cmd.map TrackSelectionMsg (TrackSelection.fetchData sharedState)
                , SharedState.NoUpdate
                )

            HideTracksModal ->
                ( { model | trackSelectionVisibility = Modal.hidden }
                , Cmd.none
                , SharedState.NoUpdate
                )

            TrackSelectionMsg tmsg ->
                let
                    ( newModel, newMsg ) =
                        TrackSelection.update TrackSelectionMsg tmsg model.trackSelection sharedState

                    ( newCoordinates, unsubscribeMsgs ) =
                        unsubscribeNewPositions model tmsg
                in
                ( { model | trackSelection = newModel }
                , Cmd.batch
                    (sendTracksToMap newCoordinates
                        :: newMsg
                        :: List.append (subscribeNewPositions model sharedState tmsg) unsubscribeMsgs
                    )
                , SharedState.NoUpdate
                )

            NoOp ->
                ( model, Cmd.none, SharedState.NoUpdate )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ MapView.mapView MapViewMsg (mapViewWithButtons model.map) [ modalView model |> fromUnstyled ] |> SHtml.toUnstyled
        ]


modalView : Model -> Html.Html Msg
modalView model =
    Modal.config HideTracksModal
        |> Modal.large
        |> Modal.h5 [] [ Html.text "Select tracks for live view" ]
        |> Modal.body [ Attributes.style "max-height" "50vh", Attributes.style "overflow" "auto" ]
            [ Grid.container []
                [ Grid.row []
                    [ Grid.col
                        [ Col.attrs [] ]
                        [ Html.map TrackSelectionMsg (TrackSelection.view model.trackSelection) ]
                    ]
                ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.attrs [ Events.onClick HideTracksModal ]
                ]
                [ Html.text "Close" ]
            ]
        |> Modal.view model.trackSelectionVisibility


insertCoordinatesToTrack : Model -> Coordinates -> Tracks
insertCoordinatesToTrack model coords =
    Maybe.map
        (\selectedTracks ->
            -- track was selected by user
            if Dict.member coords.trackId selectedTracks then
                insertCoordinatesToTrack_ model.tracks coords

            else
                model.tracks
        )
        model.trackSelection.selectedTracks
        |> Maybe.withDefault Dict.empty


insertCoordinatesToTrack_ : Tracks -> Coordinates -> Tracks
insertCoordinatesToTrack_ tracks coords =
    let
        track =
            Dict.get (String.fromInt coords.trackId) tracks
    in
    case track of
        Just t ->
            Dict.insert (String.fromInt coords.trackId) (coords :: t) tracks

        Nothing ->
            Dict.insert (String.fromInt coords.trackId) [ coords ] tracks


sendTracksToMap : Dict.Dict String (List Coordinates) -> Cmd Msg
sendTracksToMap tracks =
    Dict.toList tracks |> Ports.updateTracks


unsubscribeNewPositions : Model -> TrackSelection.Msg -> ( Tracks, List (Cmd Msg) )
unsubscribeNewPositions model tmsg =
    case tmsg of
        TrackSelection.TrackSelectToggle track isChecked ->
            if not isChecked then
                ( Dict.remove (String.fromInt track.id) model.tracks
                , [ Ports.unsubscribeCoordinates track.id ]
                )

            else
                ( model.tracks, [ Cmd.none ] )

        TrackSelection.SelectAllToggle isChecked ->
            if not isChecked then
                let
                    allTracksAsList =
                        TrackSelection.allTracksAsDict model.trackSelection |> Maybe.map Dict.values

                    newCoordinates =
                        Dict.empty
                in
                ( newCoordinates
                , allTracksAsList
                    |> Maybe.map (\t -> List.map (\tr -> Ports.unsubscribeCoordinates tr.id) t)
                    |> Maybe.withDefault [ Cmd.none ]
                )

            else
                ( model.tracks, [ Cmd.none ] )

        _ ->
            ( model.tracks, [ Cmd.none ] )


subscribeNewPositions : Model -> SharedState.SharedState -> TrackSelection.Msg -> List (Cmd Msg)
subscribeNewPositions model sharedState tmsg =
    case tmsg of
        TrackSelection.TrackSelectToggle track isChecked ->
            if isChecked then
                [ Ports.subscribeCoordinates track.id, fetchInitalCoordinates track sharedState ]

            else
                [ Cmd.none ]

        TrackSelection.SelectAllToggle isChecked ->
            if isChecked then
                let
                    allTracksAsList =
                        TrackSelection.allTracksAsDict model.trackSelection |> Maybe.map Dict.values
                in
                allTracksAsList
                    |> Maybe.map (\t -> List.map (\tr -> Ports.subscribeCoordinates tr.id) t)
                    |> Maybe.withDefault [ Cmd.none ]

            else
                [ Cmd.none ]

        TrackSelection.ZoomTo track ->
            let
                coordinate =
                    model.tracks
                        |> Dict.get (String.fromInt track.id)
            in
            case coordinate of
                Just coord ->
                    Maybe.map
                        (\x ->
                            [ Ports.centerMap x ]
                        )
                        (List.head coord)
                        |> Maybe.withDefault [ Cmd.none ]

                Nothing ->
                    [ Cmd.none ]

        _ ->
            [ Cmd.none ]
