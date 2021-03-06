module Pages.Trackers exposing (..)

import Api as Api
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Modal as Modal
import Bootstrap.Table as Table
import Html as Html
import Html.Attributes as Attributes
import Html.Events as Events
import Pages.Partials.LoadingView as Loading
import Pages.Partials.PairingView as PairingView
import RemoteData as RD
import Routing.Helpers as Helpers
import SharedState
import Types as Types


type alias Model =
    { list : RD.WebData (List Types.Tracker)
    , pairingView : PairingView.Model Msg
    , pairingViewVisibility : Modal.Visibility
    , pairingResult : Maybe (Result String String)
    , createAndPairResult : Maybe (Result String Types.Tracker)
    }


type Msg
    = HandleTrackers (RD.WebData (List Types.Tracker))
    | RequestPairing Types.Tracker
    | PairingViewMsg PairingView.Msg
    | CloseModal
    | Pair Helpers.TrackerId Helpers.TrackId
    | CreateAndPair Helpers.TrackerId String
    | NavigateTo String
    | PairingError
    | PairingResult (RD.WebData String)
    | CreateAndPairResult (RD.WebData Types.Tracker)


initModel : Model
initModel =
    let
        pairingViewModel =
            PairingView.initModel
    in
    { list = RD.Loading
    , pairingView = pairingViewModel
    , pairingViewVisibility = Modal.hidden
    , pairingResult = Nothing
    , createAndPairResult = Nothing
    }


fetchData : SharedState.SharedState -> Cmd Msg
fetchData sharedState =
    Api.fetchTrackers
        sharedState
        HandleTrackers


view : Model -> Html.Html Msg
view model =
    case model.list of
        RD.Success data ->
            let
                rows =
                    List.map
                        (\tracker ->
                            Table.tr []
                                [ Table.td [] [ Html.text <| String.fromInt tracker.id ]
                                , Table.td [] [ Html.text tracker.name ]
                                , Table.td [] [ Html.text tracker.track.name ]
                                , Table.td [] [ operations tracker ]
                                ]
                        )
                        data
            in
            Html.div [ Attributes.class "conteiner" ]
                [ modalView model
                , notificationView model
                , Table.table
                    { options = [ Table.striped, Table.responsive, Table.hover ]
                    , thead =
                        Table.simpleThead
                            [ Table.th [] [ Html.text "ID" ]
                            , Table.th [] [ Html.text "Name" ]
                            , Table.th [] [ Html.text "Paired track" ]
                            , Table.th [] [ Html.text "Operations" ]
                            ]
                    , tbody = Table.tbody [] rows
                    }
                ]

        RD.Loading ->
            Loading.view

        _ ->
            Html.div [] [ Html.text "No data" ]


update : (Msg -> msg) -> SharedState.SharedState -> Msg -> Model -> ( Model, Cmd msg, SharedState.SharedStateUpdate )
update wrapper sharedState msg model =
    (\( m, ms, s ) -> ( m, Cmd.map wrapper ms, s )) <|
        case msg of
            HandleTrackers webData ->
                ( { model | list = webData }
                , Cmd.none
                , SharedState.NoUpdate
                )

            NavigateTo _ ->
                ( model, Cmd.none, SharedState.NoUpdate )

            RequestPairing tracker ->
                let
                    ( updatedPairingViewModel, pairingMsg ) =
                        PairingView.selectTracker model.pairingView (Helpers.TrackerId tracker.id)
                in
                ( { model | pairingViewVisibility = Modal.shown, pairingView = updatedPairingViewModel }
                , Cmd.batch
                    [ Cmd.map PairingViewMsg (PairingView.fetchData sharedState)
                    , pairingMsg
                    ]
                , SharedState.NoUpdate
                )

            CloseModal ->
                ( { model | pairingViewVisibility = Modal.hidden, pairingView = PairingView.initModel }
                , Cmd.none
                , SharedState.NoUpdate
                )

            PairingViewMsg pmsg ->
                let
                    ( updatedModel, newMsg ) =
                        PairingView.update pmsg model.pairingView
                in
                ( { model | pairingView = updatedModel }, newMsg, SharedState.NoUpdate )

            Pair trackerId trackId ->
                let
                    pairMsg =
                        Api.trackAssing sharedState PairingResult trackId trackerId
                in
                ( model, pairMsg, SharedState.NoUpdate )

            CreateAndPair tracker newName ->
                let
                    pairMsg =
                        Api.trackCreate sharedState CreateAndPairResult tracker newName
                in
                ( model, pairMsg, SharedState.NoUpdate )

            PairingResult result ->
                case result of
                    RD.Success response ->
                        ( { model | pairingResult = Just (Ok response), pairingViewVisibility = Modal.hidden }, fetchData sharedState, SharedState.NoUpdate )

                    _ ->
                        ( { model | pairingResult = Just (Err "Error while pairing tracker with track") }, Cmd.none, SharedState.NoUpdate )

            CreateAndPairResult result ->
                case result of
                    RD.Success response ->
                        ( { model | createAndPairResult = Just (Ok response), pairingViewVisibility = Modal.hidden }, Cmd.none, SharedState.NoUpdate )

                    _ ->
                        ( { model | createAndPairResult = Just (Err "Error while creating & pairing tracker with track") }, Cmd.none, SharedState.NoUpdate )

            PairingError ->
                ( { model | pairingResult = Just (Err "User select invalid data") }, Cmd.none, SharedState.NoUpdate )


operations : Types.Tracker -> Html.Html Msg
operations tracker =
    Button.button [ Button.primary, Button.small, Button.onClick (RequestPairing tracker) ] [ Html.text "Add/assing track" ]


modalView : Model -> Html.Html Msg
modalView model =
    Modal.config CloseModal
        |> Modal.small
        |> Modal.h5 [] [ Html.text "Pair tracker with track" ]
        |> Modal.body []
            [ Grid.containerFluid []
                [ Grid.row []
                    [ Grid.col
                        []
                        [ Html.map PairingViewMsg (PairingView.view model.pairingView) ]
                    ]
                ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.attrs [ Events.onClick CloseModal ]
                ]
                [ Html.text "Close" ]
            , Button.button
                [ Button.primary
                , Button.attrs
                    [ Events.onClick <|
                        let
                            selected =
                                Maybe.map2 (\track tracker -> Pair track tracker) model.pairingView.selectedTracker model.pairingView.selectedTrack

                            cmd =
                                Maybe.map2 (\tracker trackName -> CreateAndPair tracker trackName) model.pairingView.selectedTracker model.pairingView.newTrackName
                        in
                        case ( selected, cmd ) of
                            ( Just msg, Nothing ) ->
                                msg

                            ( Nothing, Just msg ) ->
                                msg

                            _ ->
                                PairingError
                    ]
                ]
                [ Html.text "Save" ]
            ]
        |> Modal.view model.pairingViewVisibility


notificationView : Model -> Html.Html Msg
notificationView model =
    Html.div
        []
        [ case model.pairingResult of
            Just pairingResult ->
                case pairingResult of
                    Err err ->
                        Alert.simpleDanger [] [ Html.text err ]

                    Ok res ->
                        Alert.simpleSuccess [] [ Html.text ("Tracker successfully paired with track " ++ res) ]

            Nothing ->
                Html.div [] []
        , case model.createAndPairResult of
            Just pairingResult ->
                case pairingResult of
                    Err err ->
                        Alert.simpleDanger [] [ Html.text err ]

                    Ok res ->
                        Alert.simpleSuccess [] [ Html.text ("Track (" ++ String.fromInt res.track.id ++ ") succesfully created and paired with tracker (" ++ String.fromInt res.id ++ ")") ]

            Nothing ->
                Html.div [] []
        ]
