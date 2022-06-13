module Pages.Home exposing (Model, Msg(..), initModel, update, view)

import Api as Api exposing (fetchActiveTrackers)
import Browser.Navigation exposing (pushUrl)
import Css exposing (..)
import Decoders as Decoders
import Dict as Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (id, css)
import Html.Styled.Events exposing (..)
import Json.Decode exposing (Decoder)
import Pages.Partials.MapView as MapView
import Ports as Ports
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Http
import Routing.Helpers as Helpers exposing (Route(..), routeToString)
import SharedState as SharedState
import Types as Types exposing (Coordinates)

type alias Model =
    { coordinates : WebData (Dict.Dict String Coordinates)
    , map : MapView.Model Msg
    }


type Msg
    = NoOp
    | NewCoordinates Types.Coordinates
    | HandlePositons (WebData (Dict.Dict String Coordinates))
    | NavigateTo Helpers.Route
    | MapViewMsg MapView.Msg


initModel : Model
initModel =
    { coordinates = Success Dict.empty
    , map = MapView.initModel
    }


get : String -> (WebData success -> msg) -> Decoder success -> Cmd msg
get =
    Http.getWithConfig Http.defaultConfig


fetchInitalCoordinates : Cmd Msg
fetchInitalCoordinates =
    Api.fetchInitalCoordinates
        HandlePositons
        Decoders.decodeCoordinatesAsDict


trackSelection : Html Msg
trackSelection =
    button
        [ id "center"
        , css
            [ backgroundImage (url "drop.svg")
            , backgroundRepeat noRepeat
            , backgroundSize (px 25)
            , width (px 25)
            , height (px 25)
            , backgroundPosition center
            ]
        ]
        []

view : SharedState.SharedState -> Model -> Html Msg
view sharedState model =
    div []
        [ MapView.mapView MapViewMsg model.map [trackSelection]

        -- [ button [ id "back", NavigateTo Helpers.Tracks |> onClick ] [ text "List of tracks" ]
        -- ]
        ]


update : (Msg -> msg) -> SharedState.SharedState -> Msg -> Model -> ( Model, Cmd msg, SharedState.SharedStateUpdate )
update wrapper sharedState msg model =
    (\( m, ms, s ) -> ( m, Cmd.map wrapper ms, s )) <|
        case msg of
            NewCoordinates coords ->
                let
                    updatedCoordinates =
                        RemoteData.map (\x -> Dict.insert (String.fromInt coords.trackId) coords x) model.coordinates
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
                    ( mapViewModel, mapViewMsg ) =
                        MapView.update mapMsg model.map
                in
                ( { model | map = mapViewModel }, mapViewMsg, SharedState.NoUpdate )

            NoOp ->
                ( model, Cmd.none, SharedState.NoUpdate )


sendCoordinatesToMap : List Coordinates -> Cmd Msg
sendCoordinatesToMap coords =
    Ports.updateCoordinates coords
