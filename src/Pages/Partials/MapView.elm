module Pages.Partials.MapView exposing (Model, Msg, mapView, update, initModel)

import Css exposing (..)
import Html.Styled as SHtml exposing (button, div, node)
import Html.Styled.Attributes exposing (css, id)
import Html.Styled.Events exposing (onClick)
import Ports as Ports


type Msg
    = ToggleFullscreen
    | NoOp


type Model
    = Fullscreen
    | Normal

initModel : Model
initModel = Normal

update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case ( msg, model ) of
        ( ToggleFullscreen, Normal ) ->
            ( Fullscreen, Ports.fullscreenMap True )

        ( ToggleFullscreen, Fullscreen ) ->
            ( Normal, Ports.fullscreenMap False )

        ( NoOp, _ ) ->
            ( model, Cmd.none )


mapView : (Msg -> msg) -> List (SHtml.Html Msg) -> SHtml.Html msg
mapView wrapper att =
    SHtml.map wrapper <|
        node "seznam-maps"
            [ id "maps" ]
            [ div
                [ id "map"
                , css
                    [ height (vh 100)
                    , width (vw 100)
                    ]
                ]
                (fullscreenButton :: att)
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
