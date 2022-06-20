module Pages.UserDetails exposing (view)

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Html
import Types


view : Types.UserInfo -> Html.Html msg
view userInfo =
    Card.config [ Card.outlineInfo ]
        |> Card.headerH1 [] [ Html.text "User details" ]
        |> Card.block []
            [ Block.text []
                [ Html.div [] [ Html.text "email: ", Html.text userInfo.email ]
                , Html.div [] [ Html.text "name: ", Html.text userInfo.name ]
                ]
            ]
        |> Card.view
