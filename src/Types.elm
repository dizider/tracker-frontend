module Types exposing (..)

import OAuth
import OAuth.Implicit as OAuth
import Url exposing (Protocol(..), Url)


type alias RawFlags =
    { state : Maybe (List Int)
    , clientId : String
    , token: Maybe String
    }


type alias Flags =
    { state : Maybe String
    , clientId : String
    , token: Maybe String
    }



--
-- Model
--


type alias Model =
    { auth : AuthModel
    , clientId : String
    }


type alias AuthModel =
    { redirectUri : Url
    , flow : AuthFlow
    , token : Maybe OAuth.Token
    }


type AuthFlow
    = Idle
    | Authorized OAuth.Token
    | Done UserInfo
    | Errored Error


type alias UserInfo =
    { name : String
    , picture : String
    , email : String
    }


type Error
    = ErrStateMismatch
    | ErrAuthorization OAuth.AuthorizationError
    | ErrHTTPGetUserInfo
