module Pages.Partials.PairingView exposing (Model, Msg, Tab(..), fetchData, initModel, selectTracker, update, view)

import Api as Api
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Utilities.Flex as Flex
import Html as Html
import Html.Attributes exposing (value)
import Pages.Partials.LoadingView as Loading
import RemoteData as RD
import Routing.Helpers as Helpers
import SharedState as SharedState
import Types as Types


type Tab
    = ExistingTrack
    | NewTrack


type alias Model msg =
    { trackers : RD.WebData (List Types.Tracker)
    , tracks : RD.WebData (List Types.Track)
    , selectedTrack : Maybe Helpers.TrackId
    , selectedTracker : Maybe Helpers.TrackerId
    , newTrackName : Maybe String
    , selectedTab : Tab
    , onCreate : Maybe (String -> msg)
    }


type Msg
    = HandleTracks (RD.WebData (List Types.Track))
    | HandleTrackers (RD.WebData (List Types.Tracker))
    | SelectedTrack String
    | SelectedTracker Helpers.TrackerId
    | NewTrackNameUpdate String
    | SwitchTab


initModel : Model msg
initModel =
    { trackers = RD.Loading
    , tracks = RD.Loading
    , selectedTrack = Maybe.Nothing
    , selectedTracker = Maybe.Nothing
    , newTrackName = Nothing
    , selectedTab = ExistingTrack
    , onCreate = Nothing
    }


fetchData : SharedState.SharedState -> Cmd Msg
fetchData sharedState =
    Cmd.batch
        [ Api.fetchTracks
            sharedState
            HandleTracks
        , Api.fetchTrackers
            sharedState
            HandleTrackers
        ]


selectTracker : Model msg -> Helpers.TrackerId -> ( Model msg, Cmd msg )
selectTracker model trackerId =
    update (SelectedTracker trackerId) { model | selectedTrack = Nothing, newTrackName = Nothing }


update : Msg -> Model msg -> ( Model msg, Cmd msg )
update msg model =
    case msg of
        HandleTracks tracks ->
            ( { model | tracks = tracks }, Cmd.none )

        HandleTrackers trackers ->
            ( { model | trackers = trackers }, Cmd.none )

        SelectedTrack trackString ->
            let
                trackId =
                    Maybe.map Helpers.TrackId <| String.toInt trackString
            in
            ( { model | selectedTrack = trackId, newTrackName = Nothing }, Cmd.none )

        SelectedTracker selected ->
            ( { model | selectedTracker = Just selected }, Cmd.none )

        NewTrackNameUpdate newName ->
            if String.isEmpty newName then
                ( { model | newTrackName = Nothing, selectedTrack = Nothing }, Cmd.none )

            else
                ( { model | newTrackName = Just newName, selectedTrack = Nothing }, Cmd.none )

        SwitchTab ->
            ( { model
                | selectedTab =
                    if model.selectedTab == ExistingTrack then
                        NewTrack

                    else
                        ExistingTrack
              }
            , Cmd.none
            )


view : Model msg -> Html.Html Msg
view model =
    Html.div []
        [ case model.selectedTab of
            ExistingTrack ->
                existingTrackTab model

            NewTrack ->
                newTrackTab
        ]


existingTrackTab : Model msg -> Html.Html Msg
existingTrackTab model =
    case model.tracks of
        RD.Success a ->
            let
                filteredTracks =
                    case model.selectedTracker of
                        Just tracker ->
                            List.filter (\track -> (Helpers.TrackerId track.trackerId) == tracker) a

                        Nothing ->
                            a
            in
            Html.div []
                [ Form.form []
                    [ Form.group []
                        [ Form.label [] [ Html.text "Select one of the existing tracks" ]
                        , Select.select [ Select.id "track", Select.onChange SelectedTrack ] <|
                            Select.item [ value "" ] []
                                :: List.map (\x -> Select.item [ value <| String.fromInt x.id ] [ Html.text x.name ]) filteredTracks
                        ]
                    , Button.button [ Button.light, Button.attrs [ Flex.alignItemsCenter, Flex.alignSelfCenter ], Button.onClick SwitchTab ]
                        [ Html.text "Create new track" ]
                    ]
                ]

        RD.Loading ->
            Loading.view

        _ ->
            Html.div [] [ Html.text "No existing tracks" ]


newTrackTab : Html.Html Msg
newTrackTab =
    Html.div []
        [ Form.form []
            [ Form.group []
                [ Form.label [] [ Html.text "Name of new track" ]
                , Input.text [ Input.id "name", Input.onInput NewTrackNameUpdate ]
                ]
            ]
        , Button.button [ Button.light, Button.attrs [ Flex.alignItemsCenter, Flex.alignSelfCenter ], Button.onClick SwitchTab ]
            [ Html.text "Show existing tracks" ]
        ]
