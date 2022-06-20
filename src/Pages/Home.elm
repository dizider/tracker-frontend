module Pages.Home exposing (Model, Msg(..), initModel, update, view)

import Api as Api exposing (fetchActiveTrackers)
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Modal as Modal
import Browser.Navigation exposing (pushUrl)
import Css exposing (..)
import Decoders as Decoders
import Dict as Dict
import Html as Html
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Styled as SHtml exposing (..)
import Html.Styled.Attributes as SAttributes exposing (css, hidden, id)
import Html.Styled.Events exposing (..)
import Icons
import Json.Decode exposing (Decoder)
import Pages.Partials.MapView as MapView
import Pages.Partials.TrackSelection as TrackSelection
import Ports as Ports
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Http
import Routing.Helpers as Helpers exposing (Route(..), TrackId, routeToString)
import SharedState as SharedState
import Types as Types exposing (Coordinates)


type alias Model =
    { coordinates : WebData (Dict.Dict String Coordinates)
    , map : MapView.Model Msg
    , trackSelectionVisibility : Modal.Visibility
    , trackSelection : TrackSelection.Model
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
    }


mapViewWithButtons : MapView.Model Msg -> SharedState.SharedState -> MapView.Model Msg
mapViewWithButtons model sharedState =
    MapView.addButton (trackSelection sharedState) model


fetchInitalCoordinates : Cmd Msg
fetchInitalCoordinates =
    Api.fetchInitalCoordinates
        HandlePositons
        Decoders.decodeCoordinatesAsDict


trackSelection : SharedState.SharedState -> List (Attribute Msg) -> Html Msg
trackSelection sharedState attrs =
    button
        (List.append
            [ onClick ShowTracksModal
            , SAttributes.hidden (sharedState.viewState == SharedState.Fullscreen)
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
        [ modalView model
        , MapView.mapView MapViewMsg (mapViewWithButtons model.map sharedState) [] |> SHtml.toUnstyled
        ]


modalView : Model -> Html.Html Msg
modalView model =
    Modal.config HideTracksModal
        |> Modal.small
        |> Modal.h5 [] [ Html.text "Select tracks for live view" ]
        |> Modal.body []
            [ Grid.containerFluid []
                [ Grid.row []
                    [ Grid.col
                        []
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
                in
                case updatedCoordinates of
                    Success coordinates ->
                        ( { model | coordinates = updatedCoordinates }
                        , Dict.values coordinates |> sendCoordinatesToMap
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
                ( model, pushUrl sharedState.navKey (routeToString route), SharedState.NoUpdate )

            MapViewMsg mapMsg ->
                let
                    ( mapViewModel, mapViewMsg, sharedStateUpdate ) =
                        MapView.update mapMsg model.map sharedState
                in
                ( { model | map = mapViewModel }, mapViewMsg, sharedStateUpdate )

            ShowTracksModal ->
                ( { model | trackSelectionVisibility = Modal.shown }, Cmd.map TrackSelectionMsg TrackSelection.fetchData, SharedState.NoUpdate )

            HideTracksModal ->
                ( { model | trackSelectionVisibility = Modal.hidden }, Cmd.none, SharedState.NoUpdate )

            TrackSelectionMsg tmsg ->
                let
                    ( newModel, newMsg ) =
                        TrackSelection.update TrackSelectionMsg tmsg model.trackSelection

                    ( newCoordinates, unsubscribeMsgs ) =
                        unsubscribeNewPositions model tmsg
                in
                ( { model | trackSelection = newModel, coordinates = newCoordinates }
                , Cmd.batch (sendCoordinatesToMap_ newCoordinates :: newMsg :: List.append (subscribeNewPositions model tmsg) unsubscribeMsgs)
                , SharedState.NoUpdate
                )

            NoOp ->
                ( model, Cmd.none, SharedState.NoUpdate )


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


unsubscribeNewPositions : Model -> TrackSelection.Msg -> ( WebData (Dict.Dict String Coordinates), List (Cmd Msg) )
unsubscribeNewPositions model tmsg =
    case tmsg of
        TrackSelection.TrackSelectToggle track isChecked ->
            if not isChecked then
                ( RemoteData.map (\x -> Dict.remove (String.fromInt track.id) x) model.coordinates, [ Ports.unsubscribeCoordinates track.id ] )

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
                allTracksAsList |> Maybe.map (\t -> List.map (\tr -> Ports.subscribeCoordinates tr.id) t) |> Maybe.withDefault [ Cmd.none ]

            else
                [ Cmd.none ]

        _ ->
            [ Cmd.none ]
