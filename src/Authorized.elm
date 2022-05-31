module Authorized exposing (..)

import Auth
import Main as Main
import Result
import Types exposing (AuthModel)


{-| Execute request only if the authorization token is valid.
-}
authorizedRequest : Cmd Main.Msg -> AuthModel -> Result String (Cmd Main.Msg)
authorizedRequest msg model =
    Result.Err "Not implemented"

isAuthorized : AuthModel -> Bool
isAuthorized _ = False