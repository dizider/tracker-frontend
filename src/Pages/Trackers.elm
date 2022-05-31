module Pages.Trackers exposing (..)

import Decoders as Decoders
import Html
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (href, src)
import Html.Styled.Events exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Http
import SharedState as SharedState
import Types exposing (Tracker)


type alias Model =
    { listOfTrackers : WebData (List Tracker)
    }


type Msg
    = NoOp
    | HandleTrackers (WebData (List Tracker))
    | ReloadData


initModel : Model
initModel =
    { listOfTrackers = NotAsked
    }


init : ( Model, Cmd Msg )
init =
    ( { listOfTrackers = Loading
      }
    , fetchData
    )


fetchData : Cmd Msg
fetchData =
    Cmd.batch
        [ fetchTrackers
        ]


get =
    Http.getWithConfig Http.defaultConfig


fetchTrackers : Cmd Msg
fetchTrackers =
    get "https://api.github.com/repos/ohanhi/elm-taco/commits"
        HandleTrackers
        Decoders.decodeTrackerList


update : SharedState.SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
update sharedState msg model =
    ( model, Cmd.none, SharedState.NoUpdate )


view : SharedState.SharedState -> Model -> Html Msg
view sharedState model =
    div []
        [ h1 [] [ text "There will be a list o trackers" ]
        ]
