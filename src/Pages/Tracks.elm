module Pages.Tracks exposing (..)

import Api as Api
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table
import Browser.Navigation exposing (pushUrl)
import Decoders as Decoders
import Dict as Dict
import Html as Html
import Html.Attributes as Attributes
import Html.Events as Events
import Icons as Icons
import Pages.Partials.LoadingView as Loading
import RemoteData as RD
import Routing.Helpers as Helpers exposing (Route(..), TrackId, routeToString)
import SharedState as SharedState
import Types exposing (Track)


type alias Model =
    { listOfTracks : RD.WebData (List Track)
    , selectedTracks : Maybe (Dict.Dict Int Track)
    , isAllSelected : Bool
    }


type Msg
    = NoOp
    | HandleTracks (RD.WebData (List Track))
    | NavigateTo TrackId
    | ShowMultipleTracks
    | ReloadData
    | TrackerSelectToggle Track Bool
    | SelectAllToggle Bool


initModel : Model
initModel =
    { listOfTracks = RD.Loading
    , selectedTracks = Nothing
    , isAllSelected = False
    }


fetchData : SharedState.SharedState -> Cmd Msg
fetchData =
    fetchTracks


fetchTracks : SharedState.SharedState -> Cmd Msg
fetchTracks sharedState =
    Api.fetchTracks
        sharedState
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
                , fetchData sharedState
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

            SelectAllToggle state ->
                case ( state, model.listOfTracks ) of
                    ( True, RD.Success tracks ) ->
                        let
                            allTracksAsDict =
                                List.map (\track -> ( track.id, track )) tracks |> Dict.fromList
                        in
                        ( { model | isAllSelected = state, selectedTracks = Just allTracksAsDict }, Cmd.none, SharedState.NoUpdate )

                    _ ->
                        ( { model | isAllSelected = False, selectedTracks = Nothing }, Cmd.none, SharedState.NoUpdate )

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
                [ globalOperations model
                , Grid.row []
                    [ Grid.col []
                        [ Table.table
                            { options = [ Table.striped, Table.responsive, Table.hover ]
                            , thead =
                                Table.simpleThead
                                    [ Table.th []
                                        [ Checkbox.custom [ Checkbox.id "all", Checkbox.checked model.isAllSelected, Checkbox.onCheck SelectAllToggle ] ""
                                        ]
                                    , Table.th [] [ Html.text "ID" ]
                                    , Table.th [] [ Html.text "Name" ]
                                    , Table.th [] [ Html.text "Operations" ]
                                    ]
                            , tbody = Table.tbody [] rows
                            }
                        ]
                    ]
                ]

        RD.Loading ->
            Loading.view

        _ ->
            Html.div [] [ Html.text "No data" ]


globalOperations : Model -> Html.Html Msg
globalOperations model =
    Grid.row [ Row.attrs [ Attributes.class "sticky-top" ], Row.aroundLg ]
        [ Grid.col [ Col.attrs [ Attributes.style "background-color" "#f2f2f2" ] ]
            [ Html.text "Operations "
            , Html.button [ Attributes.disabled (model.selectedTracks == Nothing), Attributes.class "button", Events.onClick ShowMultipleTracks ] [ Icons.eye ]
            , Html.button [ Attributes.disabled True, Attributes.class "button" ] [ Icons.delete ]
            ]
        ]


operations : Types.Track -> Html.Html Msg
operations track =
    Html.div []
        [ Html.button [ Attributes.class "button", Events.onClick (NavigateTo (Helpers.TrackId track.id)) ] [ Icons.eye ]
        , Html.button [ Attributes.disabled True, Attributes.class "button" ] [ Icons.edit2 ]
        , Html.button [ Attributes.disabled True, Attributes.class "button" ] [ Icons.delete ]
        ]


checkbox : Types.Track -> Bool -> Html.Html Msg
checkbox track isChecked =
    Checkbox.custom [ Checkbox.id (String.fromInt track.id), Checkbox.checked isChecked, Checkbox.onCheck (TrackerSelectToggle track) ] ""
