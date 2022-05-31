module SharedState exposing (..)

import Browser.Navigation


type alias SharedState =
    { navKey : Browser.Navigation.Key
    , token : Maybe String
    , state : Maybe String
    }


type SharedStateUpdate
    = NoUpdate
    | UpdateState (Maybe String)
    | UpdateToken (Maybe String)


update : SharedState -> SharedStateUpdate -> SharedState
update sharedState sharedStateUpdate =
    case sharedStateUpdate of
        UpdateState newState ->
            { sharedState | state = newState }

        UpdateToken newToken ->
            { sharedState | token = newToken }

        NoUpdate ->
            sharedState
