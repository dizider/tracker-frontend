module Auth exposing (Msg(..), form, init, update, viewErrored)

import Browser.Navigation as Navigation exposing (Key)
import Config as Config exposing (translates)
import Delay exposing (TimeUnit(..), after)
import Helpers exposing (convertBytes, defaultHttpsUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Http
import Json.Decode as Json
import OAuth
import OAuth.Implicit as OAuth
import Ports exposing (genRandomBytes, getPersistedToken, persistToken)
import Types
import Url exposing (Protocol(..), Url)


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


{-| During the authentication flow, we'll run twice into the `init` function:

  - The first time, for the application very first run. And we proceed with the `Idle` state,
    waiting for the user (a.k.a you) to request a sign in.

  - The second time, after a sign in has been requested, the user is redirected to the
    authorization server and redirects the user back to our application, with an access
    token and other fields as query parameters.

When query params are present (and valid), we consider the user `Authorized`.

-}
init : Types.Flags -> Url -> Key -> ( Types.AuthModel, Cmd Msg )
init mflags origin navigationKey =
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
                    OAuth.tokenFromString (Maybe.withDefault "" mflags.token)

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
            case mflags.state of
                Nothing ->
                    ( { flow = Types.Errored Types.ErrStateMismatch
                      , redirectUri = redirectUri
                      , token = OAuth.tokenFromString (Maybe.withDefault "" mflags.token)
                      }
                    , clearUrl
                    )

                Just bytes ->
                    if state /= Just bytes then
                        ( { flow = Types.Errored Types.ErrStateMismatch
                          , redirectUri = redirectUri
                          , token = OAuth.tokenFromString (Maybe.withDefault "" mflags.token)
                          }
                        , clearUrl
                        )

                    else
                        ( { flow = Types.Authorized token
                          , redirectUri = redirectUri
                          , token = OAuth.tokenFromString (Maybe.withDefault "" mflags.token)
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
              , token = OAuth.tokenFromString (Maybe.withDefault "" mflags.token)
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
        "***REMOVED***"
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


signInRequested : Types.AuthModel -> ( Types.AuthModel, Cmd Msg )
signInRequested model =
    ( { model | flow = Types.Idle }
    , genRandomBytes 16
    )


gotRandomBytes : Types.AuthModel -> List Int -> ( Types.AuthModel, Cmd Msg )
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


userInfoRequested : Types.AuthModel -> OAuth.Token -> ( Types.AuthModel, Cmd Msg )
userInfoRequested model token =
    ( { model | flow = Types.Authorized token }
    , getUserInfo configuration token
    )


gotUserInfo : Types.AuthModel -> Result Http.Error Types.UserInfo -> ( Types.AuthModel, Cmd Msg )
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


signOutRequested : Types.AuthModel -> ( Types.AuthModel, Cmd Msg )
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


viewErrored : Types.Error -> List (Html Msg)
viewErrored error =
    [ div [ class "alert alert-danger" ] [ viewError error ] ]


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


noOp : Types.AuthModel -> ( Types.AuthModel, Cmd Msg )
noOp model =
    ( model, Cmd.none )


update : Msg -> Types.AuthModel -> ( Types.AuthModel, Cmd Msg )
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


isAuthorized : Types.AuthModel -> Bool
isAuthorized model =
    case model.flow of
        Types.Authorized token ->
            True

        _ ->
            False


form : List (Html.Html Msg) -> List (Html.Html Msg)
form page =
    page
        ++ [ div [ class "background" ]
                [-- div [ class "shape" ] []
                 -- , div [ class "shape" ] []
                ]
           , div [ class "login-form" ]
                [ div [ class "label" ]
                    [ Html.h3 [] [ Html.text (Config.translates "login-label") ]
                    ]
                , div [ class "label" ] [ Html.text (Config.translates "login-message") ]
                , div []
                    [ Html.button
                        [ Events.onClick SignInRequested, class "login-button" ]
                        [ Html.text (Config.translates "login-button") ]
                    ]
                ]
           ]