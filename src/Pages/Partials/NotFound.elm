module Pages.Partials.NotFound exposing (view)

import Bootstrap.Utilities.Spacing as Spacing
import Html


view : Html.Html msg
view =
    Html.h1 [ Spacing.m4 ] [ Html.strong [] [ Html.text "Page not found" ] ]
