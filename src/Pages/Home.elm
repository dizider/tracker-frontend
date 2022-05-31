module Pages.Home exposing (Model, Msg(..), initModel, view)

import Decoders as Decoders
import Html
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (attribute, id)
import Html.Styled.Events exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Http
import SharedState as SharedState
import Types exposing (Tracks)


type alias Model =
    { userName : String
    }


type Msg
    = NoOp


initModel : Model
initModel =
    { userName = "N/A"
    }


view : SharedState.SharedState -> Model -> Html Msg
view sharedState model =
    div []
        [ h1 [] [ text "Home page" ]
        , mapView []
        ]


mapView : List (Attribute msg) -> Html msg
mapView att =
    node "seznam-maps" [ id "maps", attribute "height" "100vh", attribute "width" "100vw" ] []
