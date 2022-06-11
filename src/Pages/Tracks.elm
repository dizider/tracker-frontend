module Pages.Tracks exposing (..)

import Api as Api
import Bootstrap.Button as Button
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Table as Table
import Browser.Navigation exposing (pushUrl)
import Decoders as Decoders
import Html as Html
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Styled
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events as StyledEvents
import Json.Decode exposing (Decoder)
import RemoteData as RD
import RemoteData.Http as RemoteHttp
import Routing.Helpers as Helpers exposing (Route(..), TrackId, routeToString)
import SharedState as SharedState
import Types exposing (Track)
import Bootstrap.Spinner as Spinner

type alias Model =
    { listOfTracks : RD.WebData (List Track)
    }


type Msg
    = NoOp
    | HandleTracks (RD.WebData (List Track))
    | NavigateTo TrackId
    | ReloadData


initModel : Model
initModel =
    { listOfTracks = RD.Loading
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

            NavigateTo track ->
                ( model, pushUrl sharedState.navKey (routeToString (MapPage (Just track))), SharedState.NoUpdate )

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
                                [ Table.td [] [ checkbox tracker ]
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
                            [ Table.th [] [ Button.button [ Button.primary, Button.small ] [ Html.text "Show selected" ] ]
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


checkbox : Types.Track -> Html.Html Msg
checkbox track =
    Checkbox.custom [ Checkbox.id (String.fromInt track.id) ] ""
