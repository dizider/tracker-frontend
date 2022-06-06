module Pages.Home exposing (Model, Msg(..), initModel, update, view)

import Browser.Navigation exposing (pushUrl)
import Css exposing (..)
import Decoders as Decoders
import Dict as Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (id)
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
    }


type Msg
    = NoOp
    | NewCoordinates Types.Coordinates
    | HandlePositons (WebData (Dict.Dict String Coordinates))
    | NavigateTo Helpers.Route


initModel : Model
initModel =
    { coordinates = Success Dict.empty
    }


get : String -> (WebData success -> msg) -> Decoder success -> Cmd msg
get =
    Http.getWithConfig Http.defaultConfig


fetchInitalCoordinates : Cmd Msg
fetchInitalCoordinates =
    get "https://tracker.dev.jenda.eu/tracker/positions"
        HandlePositons
        Decoders.decodeCoordinatesAsDict


view : SharedState.SharedState -> Model -> Html Msg
view sharedState model =
    div []
        [ MapView.mapView []
        , button [ id "back", NavigateTo Helpers.TracksList |> onClick ] [ text "List of tracks" ]
        ]


update : (Msg -> msg) -> SharedState.SharedState -> Msg -> Model -> ( Model, Cmd msg, SharedState.SharedStateUpdate )
update wrapper sharedState msg model =
    (\(m, ms, s) -> (m, Cmd.map wrapper ms, s)) <| 
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

        NoOp ->
            ( model, Cmd.none, SharedState.NoUpdate )


sendCoordinatesToMap : List Coordinates -> Cmd Msg
sendCoordinatesToMap coords =
    Ports.updateCoordinates coords
