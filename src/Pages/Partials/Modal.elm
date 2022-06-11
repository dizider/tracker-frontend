module Pages.Partials.Modal exposing (..)

import Css
import Html exposing (Attribute, Html, div, h1, text)
import Html.Attributes exposing (style)
import Html.Styled
import Html.Styled.Attributes exposing (css)


maskStyle : Html.Styled.Attribute msg
maskStyle =
    css
        [ Css.backgroundColor (Css.rgba 0 0 0 0.3)
        , Css.position Css.fixed
        , Css.top Css.zero
        , Css.left Css.zero
        , Css.width (Css.vw 100)
        , Css.height (Css.vh 100)
        ]


modalStyle : Html.Styled.Attribute msg
modalStyle =
    css
        [ Css.backgroundColor (Css.rgba 255 255 255 1.0)
        , Css.position Css.absolute
        , Css.top (Css.vh 50)
        , Css.left (Css.vw 50)
        , Css.height Css.auto
        , Css.maxHeight (Css.vh 80)
        , Css.width (Css.px 700)
        , Css.maxWidth (Css.vw 95)
        , Css.padding (Css.px 10)
        , Css.borderRadius (Css.px 3)
        , Css.boxShadow4 (Css.px 1) (Css.px 1) (Css.px 5) (Css.rgba 0 0 0 0.5)
        , Css.transform (Css.translate2 (Css.pct -50) (Css.pct -50))
        ]
