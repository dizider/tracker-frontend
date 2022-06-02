module Pages.Tracks exposing (..)

import Browser.Navigation exposing (pushUrl)
import Decoders as Decoders
import Html
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (class, href, src)
import Html.Styled.Events as Events exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Http
import Routing.Helpers as Helpers exposing (Route(..), TrackId, routeToString)
import SharedState as SharedState
import Types exposing (Track)


type alias Model =
    { listOfTracks : WebData (List Track)
    }


type Msg
    = NoOp
    | HandleTracks (WebData (List Track))
    | NavigateTo TrackId
    | ReloadData


initModel : Model
initModel =
    { listOfTracks = NotAsked
    }


init : ( Model, Cmd Msg )
init =
    ( { listOfTracks = Loading
      }
    , fetchData
    )


fetchData : Cmd Msg
fetchData =
    Cmd.batch
        [ fetchTracks
        ]


get =
    Http.getWithConfig Http.defaultConfig


fetchTracks : Cmd Msg
fetchTracks =
    get "***REMOVED***/tracks-list"
        HandleTracks
        Decoders.decodeTrackList


update : SharedState.SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
update sharedState msg model =
    case msg of
        ReloadData ->
            ( { model
                | listOfTracks = Loading
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
view sharedState model =
    case model.listOfTracks of
        Success data ->
            div []
                [ h1 [] [ text "Tracks" ]
                , data
                    |> List.map
                        (\track ->
                            let
                                trackId =
                                    Helpers.TrackId track.id
                            in
                            div []
                                [ button [ class "track-button", Events.onClick (NavigateTo trackId) ]
                                    [ div [] [ text track.name, p [] [ text (String.fromInt track.id) ] ] ]
                                ]
                        )
                    |> div [ class "tracks" ]
                ]

        _ ->
            div [] [ text "No data" ]
