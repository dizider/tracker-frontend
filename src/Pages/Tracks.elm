module Pages.Tracks exposing (..)

import Api as Api
import Browser.Navigation exposing (pushUrl)
import Decoders as Decoders
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events as Events exposing (..)
import Json.Decode exposing (Decoder)
import RemoteData as RD
import RemoteData.Http as RemoteHttp
import Routing.Helpers as Helpers exposing (Route(..), TrackId, routeToString)
import SharedState as SharedState
import Types exposing (Track)


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
    { listOfTracks = RD.NotAsked
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


view : SharedState.SharedState -> Model -> Html Msg
view _ model =
    case model.listOfTracks of
        RD.Success data ->
            div []
                [ data
                    |> List.map
                        (\track ->
                            let
                                trackId =
                                    Helpers.TrackId track.id
                            in
                            div [ class "p-2" ]
                                [ button [ class "track-button", Events.onClick (NavigateTo trackId) ]
                                    [ div [] [ text track.name, p [] [ text (String.fromInt track.id) ] ] ]
                                ]
                        )
                    |> div [ class "tracks d-flex flex-wrap" ]
                ]

        _ ->
            div [] [ text "No data" ]
