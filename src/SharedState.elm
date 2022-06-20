module SharedState exposing (..)

import Browser.Navigation
import Url exposing (Url)


type ViewState
    = Normal
    | Fullscreen


type alias SharedState =
    { navKey : Browser.Navigation.Key
    , token : Maybe String
    , state : Maybe String
    , url : Url
    , viewState : ViewState
    }


type SharedStateUpdate
    = NoUpdate
    | UpdateState (Maybe String)
    | UpdateToken (Maybe String)
    | ToggleFullscreen
    | FullscreenMode Bool


update : SharedState -> SharedStateUpdate -> SharedState
update sharedState sharedStateUpdate =
    case sharedStateUpdate of
        UpdateState newState ->
            { sharedState | state = newState }

        UpdateToken newToken ->
            { sharedState | token = newToken }

        ToggleFullscreen ->
            toogleFullScreen sharedState

        FullscreenMode fullscreen ->
            if fullscreen then
                { sharedState | viewState = Fullscreen }

            else
                { sharedState | viewState = Normal }

        NoUpdate ->
            sharedState


toogleFullScreen : SharedState -> SharedState
toogleFullScreen sharedState =
    let
        newViewState =
            case sharedState.viewState of
                Fullscreen ->
                    Normal

                Normal ->
                    Fullscreen
    in
    { sharedState | viewState = newViewState }
