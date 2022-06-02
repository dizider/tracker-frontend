module Pages.Partials.MapView exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id)

mapView : List (Attribute msg) -> Html msg
mapView att =
    node "seznam-maps" [ id "maps" ] [ div [ id "map", css [ height (vh 100), width (vw 100) ] ] [] ]
