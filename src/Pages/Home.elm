module Pages.Home exposing (Model, Msg(..), initModel, load, update, view)

import Api
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Browser.Navigation exposing (pushUrl)
import Css exposing (..)
import Decoders as Decoders
import Dict as Dict
import Html as Html
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Styled as SHtml exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (..)
import Icons
import Maybe.Extra
import Pages.Partials.MapView as MapView
import Pages.Partials.TrackSelection as TrackSelection
import Ports as Ports
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers as Helpers exposing (Route(..), routeToString)
import SharedState as SharedState
import Types as Types exposing (Coordinates)
import Browser.Navigation

type alias Tracks =
    Dict.Dict String (List Coordinates)


type alias Model =
    { coordinates : WebData (Dict.Dict String Coordinates)
    , map : MapView.Model Msg
    , trackSelectionVisibility : Modal.Visibility
    , trackSelection : TrackSelection.Model
    , tracks : Tracks
    }


type Msg
    = NoOp
    | NewCoordinates Types.Coordinates
    | HandlePositons (WebData (Dict.Dict String Coordinates))
    | NavigateTo Helpers.Route
    | MapViewMsg MapView.Msg
    | TrackSelectionMsg TrackSelection.Msg
    | ShowTracksModal
    | HideTracksModal


initModel : Model
initModel =
    { coordinates = Success Dict.empty
    , map = MapView.initModel
    , trackSelectionVisibility = Modal.hidden
    , trackSelection = TrackSelection.initModel
    , tracks = Dict.empty
    }


load : Cmd msg
load =
    MapView.load


mapViewWithButtons : MapView.Model Msg -> SharedState.SharedState -> MapView.Model Msg
mapViewWithButtons model sharedState =
    MapView.addButton (trackSelection sharedState) model


fetchInitalCoordinates : SharedState.SharedState -> Cmd Msg
fetchInitalCoordinates sharedState =
    Api.fetchInitalCoordinates
        sharedState
        HandlePositons


trackSelection : SharedState.SharedState -> List (Attribute Msg) -> Html Msg
trackSelection sharedState attrs =
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


view : SharedState.SharedState -> Model -> Html.Html Msg
view sharedState model =
    Html.div []
        [ MapView.mapView MapViewMsg (mapViewWithButtons model.map sharedState) [ modalView model |> fromUnstyled ] |> SHtml.toUnstyled
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


update : (Msg -> msg) -> SharedState.SharedState -> Msg -> Model -> ( Model, Cmd msg, SharedState.SharedStateUpdate )
update wrapper sharedState msg model =
    (\( m, ms, s ) -> ( m, Cmd.map wrapper ms, s )) <|
        case msg of
            NewCoordinates coords ->
                let
                    updatedCoordinates =
                        Maybe.map
                            (\selectedTracks ->
                                if Dict.member coords.trackId selectedTracks then
                                    RemoteData.map (\x -> Dict.insert (String.fromInt coords.trackId) coords x) model.coordinates

                                else
                                    model.coordinates
                            )
                            model.trackSelection.selectedTracks
                            |> Maybe.withDefault NotAsked

                    updatedTracks =
                        Maybe.map
                            (\selectedTracks ->
                                if Dict.member coords.trackId selectedTracks then
                                    insertCoordinatesToTrack model.tracks coords

                                else
                                    model.tracks
                            )
                            model.trackSelection.selectedTracks
                            |> Maybe.withDefault Dict.empty
                in
                case updatedCoordinates of
                    Success coordinates ->
                        ( { model | coordinates = updatedCoordinates, tracks = updatedTracks }
                        , Cmd.batch [ Dict.values coordinates |> sendCoordinatesToMap, sendTracksToMap updatedTracks ]
                        , SharedState.NoUpdate
                        )

                    _ ->
                        ( model
                        , Cmd.none
                        , SharedState.NoUpdate
                        )

            HandlePositons data ->
                ( { model | coordinates = data }
                , Cmd.none
                , SharedState.NoUpdate
                )

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
                ( { model | trackSelection = newModel, coordinates = newCoordinates }
                , Cmd.batch
                    (sendCoordinatesToMap_ newCoordinates
                        :: newMsg
                        :: List.append (subscribeNewPositions model tmsg) unsubscribeMsgs
                    )
                , SharedState.NoUpdate
                )

            NoOp ->
                ( model, Cmd.none, SharedState.NoUpdate )


insertCoordinatesToTrack : Tracks -> Coordinates -> Tracks
insertCoordinatesToTrack tracks coords =
    let
        track =
            Dict.get (String.fromInt coords.trackId) tracks
    in
    case track of
        Just t ->
            Dict.insert (String.fromInt coords.trackId) (coords :: t) tracks

        Nothing ->
            Dict.insert (String.fromInt coords.trackId) [ coords ] tracks


sendCoordinatesToMap_ : WebData (Dict.Dict String Coordinates) -> Cmd Msg
sendCoordinatesToMap_ coords =
    case coords of
        Success coordinates ->
            Dict.values coordinates |> Ports.updateCoordinates

        _ ->
            Cmd.none


sendCoordinatesToMap : List Coordinates -> Cmd Msg
sendCoordinatesToMap coords =
    Ports.updateCoordinates coords


sendTracksToMap : Dict.Dict String (List Coordinates) -> Cmd Msg
sendTracksToMap tracks =
    Dict.toList tracks |> Ports.updateTracks


unsubscribeNewPositions : Model -> TrackSelection.Msg -> ( WebData (Dict.Dict String Coordinates), List (Cmd Msg) )
unsubscribeNewPositions model tmsg =
    case tmsg of
        TrackSelection.TrackSelectToggle track isChecked ->
            if not isChecked then
                ( RemoteData.map (\x -> Dict.remove (String.fromInt track.id) x) model.coordinates
                , [ Ports.unsubscribeCoordinates track.id ]
                )

            else
                ( model.coordinates, [ Cmd.none ] )

        TrackSelection.SelectAllToggle isChecked ->
            if not isChecked then
                let
                    allTracksAsList =
                        TrackSelection.allTracksAsDict model.trackSelection |> Maybe.map Dict.values

                    newCoordinates =
                        RemoteData.map (always Dict.empty) model.coordinates
                in
                ( newCoordinates
                , allTracksAsList
                    |> Maybe.map (\t -> List.map (\tr -> Ports.unsubscribeCoordinates tr.id) t)
                    |> Maybe.withDefault [ Cmd.none ]
                )

            else
                ( model.coordinates, [ Cmd.none ] )

        _ ->
            ( model.coordinates, [ Cmd.none ] )


subscribeNewPositions : Model -> TrackSelection.Msg -> List (Cmd Msg)
subscribeNewPositions model tmsg =
    case tmsg of
        TrackSelection.TrackSelectToggle track isChecked ->
            if isChecked then
                [ Ports.subscribeCoordinates track.id ]

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
                    model.coordinates
                        |> RemoteData.map (\c -> Dict.get (String.fromInt track.id) c)
                        |> RemoteData.toMaybe
                        |> Maybe.Extra.join
            in
            case coordinate of
                Just coord ->
                    [ Ports.centerMap coord ]

                Nothing ->
                    [ Cmd.none ]

        _ ->
            [ Cmd.none ]
