module Pages.Tracks exposing (..)

import Decoders as Decoders
import Html
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (href, src, class)
import Html.Styled.Events exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Http
import SharedState as SharedState
import Types exposing (Tracks)


type alias Model =
    { listOfTracks : WebData (List Tracks)
    }


type Msg
    = NoOp
    | HandleTracks (WebData (List Tracks))
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
    get "https://tracker.dev.jenda.eu/tracks-list"
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

        NoOp ->
            ( model, Cmd.none, SharedState.NoUpdate )


view : SharedState.SharedState -> Model -> Html Msg
view sharedState model =
    let
        _ =
            Debug.log "" model
    in
    case model.listOfTracks of
        Success data -> 
            div []
                [ h1 [] [ text "Tracks" ]
                , data
                    |> List.map (\l -> div [] [button [class "track-button"] [ text l.name ]])
                    |> div [class "tracks"]
                ]
        _ -> div [] [text "No data"]
