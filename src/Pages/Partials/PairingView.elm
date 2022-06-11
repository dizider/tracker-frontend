module Pages.Partials.PairingView exposing (Model, Tab(..), Msg, fetchData, initModel, selectTracker, update, view)

import Api as Api
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Modal as Modal
import Html.Events as Events
import Bootstrap.Grid as Grid
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Spinner as Spinner
import Bootstrap.Utilities.Flex as Flex
import Decoders
import Html as Html
import Html.Attributes exposing (value)
import RemoteData as RD
import Routing.Helpers as Helpers
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


fetchData : Cmd Msg
fetchData =
    Cmd.batch
        [ Api.fetchTracks
            HandleTracks
            Decoders.decodeTrackList
        , Api.fetchTrackers
            HandleTrackers
            Decoders.decodeTrackerList
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
            Html.div []
                [ Form.form []
                    [ Form.group []
                        [ Form.label [] [ Html.text "Select one of the existing tracks" ]
                        , Select.select [ Select.id "track", Select.onChange SelectedTrack ] <|
                            Select.item [ value "" ] []
                                :: List.map (\x -> Select.item [ value <| String.fromInt x.id ] [ Html.text x.name ]) a
                        ]
                    , Button.button [ Button.light, Button.attrs [ Flex.alignItemsCenter, Flex.alignSelfCenter ], Button.onClick SwitchTab ]
                        [ Html.text "Create new track" ]
                    ]
                ]

        RD.Loading ->
            Spinner.spinner [ Spinner.grow, Spinner.large ] [ Spinner.srMessage "Loading..." ]

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


-- modalView : Model -> Html.Html Msg
-- modalView model =
--     Modal.config CloseModal
--         |> Modal.small
--         |> Modal.h5 [] [ Html.text "Pair tracker with track" ]
--         |> Modal.body []
--             [ Grid.containerFluid []
--                 [ Grid.row []
--                     [ Grid.col
--                         []
--                         [ Html.map PairingViewMsg (PairingView.view model.pairingView) ]
--                     ]
--                 ]
--             ]
--         |> Modal.footer []
--             [ Button.button
--                 [ Button.outlinePrimary
--                 , Button.attrs [ Events.onClick CloseModal ]
--                 ]
--                 [ Html.text "Close" ]
--             , Button.button
--                 [ Button.primary
--                 , Button.attrs
--                     [ Events.onClick <|
--                         let
--                             selected =
--                                 Maybe.map2 (\track tracker -> Pair track tracker) model.pairingView.selectedTracker model.pairingView.selectedTrack
--                         in
--                         case selected of
--                             Nothing ->
--                                 PairingError

--                             Just msg ->
--                                 msg
--                     ]
--                 ]
--                 [ Html.text "Save" ]
--             ]
--         |> Modal.view model.pairingViewVisibility