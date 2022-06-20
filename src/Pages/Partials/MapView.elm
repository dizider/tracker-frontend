module Pages.Partials.MapView exposing (Model, Msg, addButton, initModel, mapView, update)

import Css exposing (..)
import Html.Styled as SHtml exposing (button, div, fromUnstyled, node)
import Html.Styled.Attributes exposing (css, id)
import Html.Styled.Events exposing (onClick)
import Icons
import Ports as Ports
import SharedState


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


addButton : (List (SHtml.Attribute msg) -> SHtml.Html msg) -> Model msg -> Model msg
addButton btn model =
    let
        styledButton =
            btn
                [ css
                    [ position absolute
                    , display block
                    , padding3 (px 5) (px 8) (px 1)
                    , border3 (px 1) solid (rgba 107 117 128 0.3)
                    , borderRadius (px 2)
                    , color (rgb 107 117 128)
                    , backgroundColor (rgb 255 255 255)
                    , fontSize (px 14)
                    , textAlign right
                    , textDecoration none
                    , visibility visible
                    , zIndex (int 2)
                    ]
                ]
    in
    { model | additionalButtons = styledButton :: model.additionalButtons }


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
            (List.append model.additionalButtons (SHtml.map wrapper fullscreenButton :: att))
        ]


fullscreenButton : SHtml.Html Msg
fullscreenButton =
    button
        [ css
            [ position absolute
            , top (px 130)
            , right (px 17)
            , display block
            , padding3 (px 5) (px 8) (px 1)
            , border3 (px 1) solid (rgba 107 117 128 0.3)
            , borderRadius (px 2)
            , color (rgb 107 117 128)
            , backgroundColor (rgb 255 255 255)
            , fontSize (px 14)
            , textAlign right
            , textDecoration none
            , visibility visible
            , zIndex (int 2)
            ]
        , onClick ToggleFullscreen
        ]
        [ Icons.maximize |> fromUnstyled, SHtml.text " Fullscreen" ]
