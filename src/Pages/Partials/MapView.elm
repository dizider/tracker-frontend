module Pages.Partials.MapView exposing (Model, Msg, initModel, mapView, update)

import Css exposing (..)
import Html as Html
import Html.Styled as SHtml exposing (button, div, node)
import Html.Styled.Attributes exposing (css, id)
import Html.Styled.Events exposing (onClick)
import Ports as Ports


type Msg
    = ToggleFullscreen
    | NoOp


type alias Model msg =
    { additionalButtons : List (SHtml.Html msg)
    }


initModel : Model msg
initModel =
    { additionalButtons = []
    }


addButton : SHtml.Html msg -> Model msg -> Model msg
addButton button model =
    { model | additionalButtons = button :: model.additionalButtons }


update : Msg -> Model msg -> ( Model msg, Cmd msg )
update msg model =
    case msg of
        ToggleFullscreen ->
            ( model, Ports.fullscreenMap () )

        NoOp ->
            ( model, Cmd.none )


mapView : (Msg -> msg) -> Model msg -> List (SHtml.Html msg) -> SHtml.Html msg
mapView wrapper model att =
    node "seznam-maps"
        [ id "maps" ]
        [ div
            [ id "map"
            , css
                [ height (vh 100)
                , width (vw 100)
                ]
            ]
            (List.append (SHtml.map wrapper fullscreenButton :: att) model.additionalButtons)
        ]


fullscreenButton : SHtml.Html Msg
fullscreenButton =
    button
        [ id "fullscreen"
        , css
            [ backgroundImage (url "fullscreen.png")
            , backgroundRepeat noRepeat
            , backgroundSize (px 25)
            , width (px 25)
            , height (px 25)
            , backgroundPosition center
            ]
        , onClick ToggleFullscreen
        ]
        []
