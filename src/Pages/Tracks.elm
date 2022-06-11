module Pages.Tracks exposing (..)

import Api as Api
import Bootstrap.Button as Button
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Spinner as Spinner
import Bootstrap.Table as Table
import Browser.Navigation exposing (pushUrl)
import Decoders as Decoders
import Dict as Dict
import Html as Html
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Styled
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events as StyledEvents
import Json.Decode exposing (Decoder)
import Pages.Map as Map
import RemoteData as RD
import RemoteData.Http as RemoteHttp
import Routing.Helpers as Helpers exposing (Route(..), TrackId, routeToString)
import SharedState as SharedState
import Types exposing (Track)


type alias Model =
    { listOfTracks : RD.WebData (List Track)
    , selectedTracks : Maybe (Dict.Dict Int Track)
    }


type Msg
    = NoOp
    | HandleTracks (RD.WebData (List Track))
    | NavigateTo TrackId
    | ShowMultipleTracks
    | ReloadData
    | TrackerSelectToggle Track Bool


initModel : Model
initModel =
    { listOfTracks = RD.Loading
    , selectedTracks = Nothing
    }


fetchData : Cmd Msg
fetchData =
    Cmd.batch
        [ fetchTracks
        ]


fetchTracks : Cmd Msg
fetchTracks =
    Api.fetchTracks
        HandleTracks
        Decoders.decodeTrackList


update : (Msg -> msg) -> SharedState.SharedState -> Msg -> Model -> ( Model, Cmd msg, SharedState.SharedStateUpdate )
update wrapper sharedState msg model =
    (\( m, ms, s ) -> ( m, Cmd.map wrapper ms, s )) <|
        case msg of
            ReloadData ->
                ( { model
                    | listOfTracks = RD.Loading
                  }
                , fetchData
                , SharedState.NoUpdate
                )

            HandleTracks webData ->
                ( { model | listOfTracks = webData }
                , Cmd.none
                , SharedState.NoUpdate
                )

            TrackerSelectToggle track isChecked ->
                let
                    selectedTracks =
                        if isChecked then
                            case model.selectedTracks of
                                Nothing ->
                                    Dict.fromList [ ( track.id, track ) ] |> Just

                                Just x ->
                                    Dict.insert track.id track x |> Just

                        else
                            Maybe.map (Dict.remove track.id) model.selectedTracks |> toNothing

                    toNothing =
                        Maybe.andThen
                            (\t ->
                                if Dict.isEmpty t then
                                    Nothing

                                else
                                    Just t
                            )
                in
                ( { model | selectedTracks = selectedTracks }, Cmd.none, SharedState.NoUpdate )

            NavigateTo track ->
                ( model, pushUrl sharedState.navKey (routeToString (MapPage (Just [ track ]))), SharedState.NoUpdate )

            ShowMultipleTracks ->
                let
                    tracksId =
                        model.selectedTracks |> Maybe.map (\x -> List.map (\track -> Helpers.TrackId track.id) (Dict.values x))
                in
                case tracksId of
                    Just ids ->
                        ( model, pushUrl sharedState.navKey (routeToString (MapPage (Just ids))), SharedState.NoUpdate )

                    Nothing ->
                        ( model, Cmd.none, SharedState.NoUpdate )

            NoOp ->
                ( model, Cmd.none, SharedState.NoUpdate )


view : SharedState.SharedState -> Model -> Html.Html Msg
view _ model =
    case model.listOfTracks of
        RD.Success data ->
            let
                rows =
                    List.map
                        (\tracker ->
                            Table.tr []
                                [ Table.td [] [ checkbox tracker (Maybe.withDefault Dict.empty model.selectedTracks |> Dict.member tracker.id) ]
                                , Table.td [] [ Html.text <| String.fromInt tracker.id ]
                                , Table.td [] [ Html.text tracker.name ]
                                , Table.td [] [ operations tracker ]
                                ]
                        )
                        data
            in
            Html.div []
                [ Table.table
                    { options = [ Table.striped, Table.responsive, Table.hover ]
                    , thead =
                        Table.simpleThead
                            [ Table.th [] [ Button.button [  Button.disabled (model.selectedTracks == Nothing), Button.primary, Button.small, Button.onClick ShowMultipleTracks ] [ Html.text "Show selected" ] ]
                            , Table.th [] [ Html.text "ID" ]
                            , Table.th [] [ Html.text "Name" ]
                            , Table.th [] [ Html.text "Operations" ]
                            ]
                    , tbody = Table.tbody [] rows
                    }
                ]

        RD.Loading ->
            Spinner.spinner [ Spinner.grow, Spinner.large ] [ Spinner.srMessage "Loading..." ]

        _ ->
            Html.div [] [ Html.text "No data" ]


operations : Types.Track -> Html.Html Msg
operations track =
    Button.button [ Button.primary, Button.small, Button.onClick (NavigateTo (Helpers.TrackId track.id)) ] [ Html.text "Show" ]


checkbox : Types.Track -> Bool -> Html.Html Msg
checkbox track isChecked =
    Checkbox.custom [ Checkbox.id (String.fromInt track.id), Checkbox.checked isChecked, Checkbox.onCheck (TrackerSelectToggle track) ] ""
