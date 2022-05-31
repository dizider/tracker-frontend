module Flags exposing (..)

import Helpers as Helpers

type alias RawFlags =
    { state : Maybe (List Int)
    , clientId : String
    , token : Maybe String
    }


type alias Flags =
    { state : Maybe String
    , clientId : String
    , token : Maybe String
    }


decodeFlags : RawFlags -> Flags
decodeFlags flags =
    let
        state =
            Maybe.map Helpers.convertBytes flags.state
    in
    { state = state
    , clientId = flags.clientId
    , token = flags.token
    }