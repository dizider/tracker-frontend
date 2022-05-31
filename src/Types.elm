module Types exposing (..)

import Dict exposing (Dict)
import OAuth
import OAuth.Implicit as OAuth
import Url exposing (Protocol(..), Url)
import Json.Decode exposing (Decoder, at, dict, field, float, int, string, succeed)
import Json.Decode.Pipeline exposing (required, requiredAt)

type alias Translations =
    Dict String String

decodeTranslations : Decoder Translations
decodeTranslations =
    dict string

type alias ViewConfiguration =
    { title : String
    }


type Page
    = NotFound
    | Trackers


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

type alias Tracks =
    { id : Int
    ,trackerId: Int
    ,name: String
    ,visitedWaypoints: Int
    }