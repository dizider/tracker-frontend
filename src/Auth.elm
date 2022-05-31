module Auth exposing (Model, Msg(..), form, init, initModel, update, viewErrored, viewIdle)

import Browser.Navigation as Navigation exposing (Key)
import Config as Config exposing (translates)
import Delay exposing (TimeUnit(..), after)
import Helpers exposing (convertBytes, defaultHttpsUrl)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, href, id)
import Html.Styled.Events as Events exposing (onClick)
import Http
import Json.Decode as Json
import OAuth
import OAuth.Implicit as OAuth
import Ports exposing (genRandomBytes, getPersistedToken, persistToken)
import SharedState as SharedState
import Types as Types
import Url exposing (Protocol(..), Url)
import Url.Builder


type alias Configuration =
    { authorizationEndpoint : Url
    , userInfoEndpoint : Url
    , userInfoDecoder : Json.Decoder Types.UserInfo
    , clientId : String
    , scope : List String
    }


type Msg
    = NoOp
    | SignInRequested
    | GotRandomBytes (List Int)
    | GotPersistedToken String
    | GotAccessToken (Result Http.Error OAuth.AuthorizationSuccess)
    | UserInfoRequested
    | GotUserInfo (Result Http.Error Types.UserInfo)
    | SignOutRequested


type alias Model =
    { redirectUri : Url
    , flow : Types.AuthFlow
    , token : Maybe OAuth.Token
    }


initModel : Url -> Model
initModel origin =
    let
        redirectUri =
            { origin | query = Nothing, fragment = Nothing }
    in
    { redirectUri = redirectUri
    , flow = Types.Idle
    , token = Nothing
    }


{-| During the authentication flow, we'll run twice into the `init` function:

  - The first time, for the application very first run. And we proceed with the `Idle` state,
    waiting for the user (a.k.a you) to request a sign in.

  - The second time, after a sign in has been requested, the user is redirected to the
    authorization server and redirects the user back to our application, with an access
    token and other fields as query parameters.

When query params are present (and valid), we consider the user `Authorized`.

-}
init : SharedState.SharedState -> Url -> Key -> ( Model, Cmd Msg )
init sharedState origin navigationKey =
    let
        redirectUri =
            { origin | query = Nothing, fragment = Nothing }

        clearUrl =
            Navigation.replaceUrl navigationKey (Url.toString redirectUri)
    in
    case OAuth.parseToken origin of
        OAuth.Empty ->
            let
                token =
                    OAuth.tokenFromString (Maybe.withDefault "" sharedState.token)

                flow =
                    Maybe.map Types.Authorized token

                _ =
                    Debug.log "Persisted token" token
            in
            -- TODO: Handling invalid token in memory
            ( { flow = Maybe.withDefault Types.Idle flow
              , redirectUri = redirectUri
              , token = token
              }
            , Cmd.batch [ after 750 Millisecond UserInfoRequested, clearUrl ]
            )

        -- It is important to set a `state` when making the authorization request
        -- and to verify it after the redirection. The state can be anything but its primary
        -- usage is to prevent cross-site request forgery; at minima, it should be a short,
        -- non-guessable string, generated on the fly.
        --
        -- We remember any previously generated state  state using the browser's local storage
        -- and give it back (if present) to the elm application upon start
        OAuth.Success { token, state } ->
            case sharedState.state of
                Nothing ->
                    ( { flow = Types.Errored Types.ErrStateMismatch
                      , redirectUri = redirectUri
                      , token = OAuth.tokenFromString (Maybe.withDefault "" sharedState.token)
                      }
                    , clearUrl
                    )

                Just bytes ->
                    if state /= Just bytes then
                        ( { flow = Types.Errored Types.ErrStateMismatch
                          , redirectUri = redirectUri
                          , token = OAuth.tokenFromString (Maybe.withDefault "" sharedState.token)
                          }
                        , clearUrl
                        )

                    else
                        ( { flow = Types.Authorized token
                          , redirectUri = redirectUri
                          , token = OAuth.tokenFromString (Maybe.withDefault "" sharedState.token)
                          }
                        , Cmd.batch
                            -- Artificial delay to make the live demo easier to follow.
                            -- In practice, the access token could be requested right here.
                            [ after 750 Millisecond UserInfoRequested
                            , persistToken (OAuth.tokenToString token)
                            , clearUrl
                            ]
                        )

        OAuth.Error error ->
            ( { flow = Types.Errored <| Types.ErrAuthorization error
              , redirectUri = redirectUri
              , token = OAuth.tokenFromString (Maybe.withDefault "" sharedState.token)
              }
            , Cmd.batch
                [ Ports.removeToken True
                , clearUrl
                ]
            )


{-| OAuth configuration.

Note that this demo also fetches basic user information with the obtained access token,
hence the user info endpoint and JSON decoder

-}
configuration : Configuration
configuration =
    { authorizationEndpoint =
        { defaultHttpsUrl | host = "accounts.google.com", path = "/o/oauth2/v2/auth" }
    , userInfoEndpoint =
        { defaultHttpsUrl | host = "www.googleapis.com", path = "/oauth2/v1/userinfo" }
    , userInfoDecoder =
        Json.map3 Types.UserInfo
            (Json.field "name" Json.string)
            (Json.field "picture" Json.string)
            (Json.field "email" Json.string)
    , clientId =
        "1033652281621-os35kv9ie4jnisbmcukrv1fcf14n41u2.apps.googleusercontent.com"
    , scope =
        [ "profile", "email", "openid" ]
    }


getUserInfo : Configuration -> OAuth.Token -> Cmd Msg
getUserInfo { userInfoDecoder, userInfoEndpoint } token =
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken token []
        , url = Url.toString userInfoEndpoint
        , expect = Http.expectJson GotUserInfo userInfoDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


signInRequested : Model -> ( Model, Cmd Msg )
signInRequested model =
    ( { model | flow = Types.Idle }
    , genRandomBytes 16
    )


gotRandomBytes : Model -> List Int -> ( Model, Cmd Msg )
gotRandomBytes model bytes =
    let
        state =
            Helpers.convertBytes bytes

        authorization =
            { clientId = configuration.clientId
            , redirectUri = model.redirectUri
            , scope = configuration.scope
            , state = Just state
            , url = configuration.authorizationEndpoint
            }
    in
    ( { model | flow = Types.Idle }
    , authorization
        |> OAuth.makeAuthorizationUrl
        |> Url.toString
        |> Navigation.load
    )


userInfoRequested : Model -> OAuth.Token -> ( Model, Cmd Msg )
userInfoRequested model token =
    ( { model | flow = Types.Authorized token }
    , getUserInfo configuration token
    )


gotUserInfo : Model -> Result Http.Error Types.UserInfo -> ( Model, Cmd Msg )
gotUserInfo model userInfoResponse =
    case userInfoResponse of
        Err _ ->
            ( { model | flow = Types.Errored Types.ErrHTTPGetUserInfo }
            , Ports.removeToken True
            )

        Ok userInfo ->
            ( { model | flow = Types.Done userInfo }
            , Cmd.none
            )


signOutRequested : Model -> ( Model, Cmd Msg )
signOutRequested model =
    ( { model | flow = Types.Idle }
    , Navigation.load (Url.toString model.redirectUri)
    )


oauthErrorToString : { error : OAuth.ErrorCode, errorDescription : Maybe String } -> String
oauthErrorToString { error, errorDescription } =
    let
        desc =
            errorDescription |> Maybe.withDefault "" |> String.replace "+" " "
    in
    OAuth.errorCodeToString error ++ ": " ++ desc


view : Model -> Html Msg
view model =
    case model.flow of
        Types.Errored err ->
            viewErrored err
        -- Types.Done userInfo ->
            -- view
        _ -> 
            viewIdle

viewIdle : Html Msg
viewIdle =
    form


viewErrored : Types.Error -> Html Msg
viewErrored error =
    div [ class "alert alert-danger" ] [ viewError error ]


viewError : Types.Error -> Html Msg
viewError e =
    text <|
        case e of
            Types.ErrStateMismatch ->
                "'state' doesn't match, the request has likely been forged by an adversary!"

            Types.ErrAuthorization error ->
                oauthErrorToString { error = error.error, errorDescription = error.errorDescription }

            Types.ErrHTTPGetUserInfo ->
                "Unable to retrieve user info: HTTP request failed."


noOp : Model -> ( Model, Cmd Msg )
noOp model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.flow, msg ) of
        ( Types.Idle, SignInRequested ) ->
            signInRequested model

        ( Types.Idle, GotRandomBytes bytes ) ->
            gotRandomBytes model bytes

        ( Types.Authorized token, UserInfoRequested ) ->
            userInfoRequested model token

        ( Types.Authorized _, GotUserInfo userInfoResponse ) ->
            gotUserInfo model userInfoResponse

        ( Types.Done _, SignOutRequested ) ->
            signOutRequested model

        _ ->
            noOp model


isAuthorized : Model -> Bool
isAuthorized model =
    case model.flow of
        Types.Authorized token ->
            True

        _ ->
            False


form : Html Msg
form =
    div []
        [ div [ class "background" ]
            [-- div [ class "shape" ] []
             -- , div [ class "shape" ] []
            ]
        , div [ class "login-form" ]
            [ div [ class "label" ]
                [ h3 [] [ text (Config.translates "login-label") ]
                ]
            , div [ class "label" ] [ text (Config.translates "login-message") ]
            , div []
                [ button
                    [ Events.onClick SignInRequested, class "login-button" ]
                    [ text (Config.translates "login-button") ]
                ]
            ]
        ]
