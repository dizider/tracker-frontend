module Config exposing (..)

translates : String -> String
translates key =
    case key of
        "login-button" -> "Sign In"
        "login-message" -> "Trackers data are avalible only for logged in users"
        "login-label" -> "Authorization"
        _ -> "N/A"