module Icons exposing
    ( center
    , delete
    , edit2
    , eye
    , maximize
    )

import Html exposing (Html)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ class <| "feather feather-" ++ className
        , fill "none"
        , height "24"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        , viewBox "0 0 24 24"
        , width "24"
        ]


center : Html msg
center =
    svgFeatherIcon "center"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "22", y1 "12", x2 "18", y2 "12" ] []
        , Svg.line [ x1 "6", y1 "12", x2 "2", y2 "12" ] []
        , Svg.line [ x1 "12", y1 "6", x2 "12", y2 "2" ] []
        , Svg.line [ x1 "12", y1 "22", x2 "12", y2 "18" ] []
        ]


delete : Html msg
delete =
    svgFeatherIcon "delete"
        [ Svg.path [ d "M21 4H8l-7 8 7 8h13a2 2 0 0 0 2-2V6a2 2 0 0 0-2-2z" ] []
        , Svg.line [ x1 "18", y1 "9", x2 "12", y2 "15" ] []
        , Svg.line [ x1 "12", y1 "9", x2 "18", y2 "15" ] []
        ]


edit2 : Html msg
edit2 =
    svgFeatherIcon "edit-2"
        [ Svg.path [ d "M17 3a2.828 2.828 0 1 1 4 4L7.5 20.5 2 22l1.5-5.5L17 3z" ] []
        ]


eye : Html msg
eye =
    svgFeatherIcon "eye"
        [ Svg.path [ d "M1 12s4-8 11-8 11 8 11 8-4 8-11 8-11-8-11-8z" ] []
        , Svg.circle [ cx "12", cy "12", r "3" ] []
        ]


maximize : Html msg
maximize =
    svgFeatherIcon "maximize"
        [ Svg.path [ d "M8 3H5a2 2 0 0 0-2 2v3m18 0V5a2 2 0 0 0-2-2h-3m0 18h3a2 2 0 0 0 2-2v-3M3 16v3a2 2 0 0 0 2 2h3" ] []
        ]
