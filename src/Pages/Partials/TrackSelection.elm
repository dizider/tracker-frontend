module Pages.Partials.TrackSelection exposing (Model, Msg(..), fetchData, initModel, update, view, allTracksAsDict)

import Api as Api
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Pages.Partials.LoadingView as Loading
import Bootstrap.Table as Table
import Decoders as Decoders
import Delay exposing (TimeUnit(..))
import Dict as Dict
import Html as Html
import Html.Attributes as Attributes
import RemoteData as RD
import Types exposing (Track)
import Bootstrap.Utilities.Size as Size

type alias Model =
    { listOfTracks : RD.WebData (List Track)
    , selectedTracks : Maybe (Dict.Dict Int Track)
    , isAllSelected : Bool
    }


type Msg
    = HandleTracks (RD.WebData (List Track))
    | ReloadData
    | TrackSelectToggle Track Bool
    | SelectAllToggle Bool


initModel : Model
initModel =
    { listOfTracks = RD.Loading
    , selectedTracks = Nothing
    , isAllSelected = False
    }


fetchData : Cmd Msg
fetchData =
    Api.fetchTracks
        HandleTracks
        Decoders.decodeTrackList


update : (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update wrapper msg model =
    case msg of
        ReloadData ->
            ( { model
                | listOfTracks = RD.Loading
              }
            , fetchData |> Cmd.map wrapper
            )

        HandleTracks webData ->
            ( { model | listOfTracks = webData }
            , Cmd.none
            )

        TrackSelectToggle track isChecked ->
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
            ( { model | selectedTracks = selectedTracks }, Cmd.none )

        SelectAllToggle state ->
            if state then
                ( { model | isAllSelected = state, selectedTracks = allTracksAsDict model }, Cmd.none )

            else
                ( { model | isAllSelected = False, selectedTracks = Nothing }, Cmd.none )


view : Model -> Html.Html Msg
view model =
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
                                ]
                        )
                        data
            in
            Html.div []
                [ Grid.row []
                    [ Grid.col [ Col.lg12 ]
                        [ Table.table
                            { options = [ Table.striped, Table.hover]
                            , thead =
                                Table.simpleThead
                                    [ Table.th []
                                        [ Checkbox.custom [ Checkbox.id "all", Checkbox.checked model.isAllSelected, Checkbox.onCheck SelectAllToggle ] ""
                                        ]
                                    , Table.th [] [ Html.text "ID" ]
                                    , Table.th [] [ Html.text "Name" ]
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


checkbox : Types.Track -> Bool -> Html.Html Msg
checkbox track isChecked =
    Checkbox.custom [ Checkbox.id (String.fromInt track.id), Checkbox.checked isChecked, Checkbox.onCheck (TrackSelectToggle track) ] ""


allTracksAsDict : Model -> Maybe (Dict.Dict Int Track)
allTracksAsDict model =
    case model.listOfTracks of
        RD.Success tracks ->
            List.map (\track -> ( track.id, track )) tracks |> Dict.fromList |> Just

        _ ->
            Nothing
