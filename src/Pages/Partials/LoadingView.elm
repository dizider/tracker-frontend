module Pages.Partials.LoadingView exposing (view)

import Bootstrap.Spinner as Spinner
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Html as Html


view : Html.Html msg
view =
    Html.div [ Flex.block, Flex.alignItemsCenter, Spacing.m4 ] [ Html.strong [] [ Html.text "Loading" ], Spinner.spinner [ Spinner.attrs [ Spacing.mlAuto ] ] [] ]
