module Types exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, dict, string)
import OAuth
import OAuth.Implicit as OAuth
import Url exposing (Protocol(..))


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


type alias Track =
    { id : Int
    , trackerId : Int
    , name : String
    , visitedWaypoints : Int
    }


type alias Tracker =
    { id : Int
    , name : String
    , track: Track
    }


type alias Coordinates =
    { id : Int
    , trackId : Int
    , time : String
    , lat : Float
    , lon : Float
    , alt : Float
    , battery : Float
    }
