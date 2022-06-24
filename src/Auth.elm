module Auth exposing (Model, Msg(..), authorizedMsg, form, init, initModel, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Grid as Grid
import Browser.Navigation as Navigation
import Config as Config
import Delay exposing (TimeUnit(..), after)
import Helpers exposing (defaultHttpsUrl)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events as Events
import Http
import Json.Decode as Json
import OAuth
import OAuth.Implicit as OAuth
import Pages.Partials.UserDetails as UserDetails
import Ports exposing (genRandomBytes, persistToken)
import Routing.Helpers as Helpers
import SharedState as SharedState
import Types as Types
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


type alias Model =
    { redirectUri : Url
    , flow : Types.AuthFlow
    , token : Maybe OAuth.Token
    , config : Configuration
    }


initModel : SharedState.SharedState -> Url -> Maybe String -> Model
initModel sharedState origin stoken =
    let
        redirectUri =
            { origin | query = Nothing, fragment = Nothing, path = Helpers.routeToString Helpers.AuthPage }
    in
    { redirectUri = redirectUri
    , flow = Types.Idle
    , token = Maybe.andThen OAuth.tokenFromString stoken
    , config = configuration sharedState
    }


init : Model -> SharedState.SharedState -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init model sharedState origin navigationKey =
    let
        redirectUri =
            { origin | query = Nothing, fragment = Nothing, path = Helpers.routeToString Helpers.AuthPage }
    in
    case OAuth.parseToken origin of
        OAuth.Empty ->
            let
                token =
                    OAuth.tokenFromString (Maybe.withDefault "" sharedState.token)

                flow =
                    Maybe.map Types.Authorized token
            in
            ( { flow = Maybe.withDefault Types.Idle flow
              , redirectUri = redirectUri
              , token = token
              , config = model.config
              }
            , after 0 Millisecond UserInfoRequested
            )

        OAuth.Success { token, state } ->
            case sharedState.state of
                Nothing ->
                    ( { flow = Types.Errored Types.ErrStateMismatch
                      , redirectUri = redirectUri
                      , token = OAuth.tokenFromString (Maybe.withDefault "" sharedState.token)
                      , config = model.config
                      }
                    , Cmd.none
                    )

                Just bytes ->
                    if state /= Just bytes then
                        ( { flow = Types.Errored Types.ErrStateMismatch
                          , redirectUri = redirectUri
                          , token = OAuth.tokenFromString (Maybe.withDefault "" sharedState.token)
                          , config = model.config
                          }
                        , Cmd.none
                        )

                    else
                        ( { flow = Types.Authorized token
                          , redirectUri = redirectUri
                          , token = OAuth.tokenFromString (Maybe.withDefault "" sharedState.token)
                          , config = model.config
                          }
                        , Cmd.batch
                            [ after 0 Millisecond UserInfoRequested
                            , persistToken (OAuth.tokenToString token)
                            ]
                        )

        OAuth.Error error ->
            ( { flow = Types.Errored <| Types.ErrAuthorization error
              , redirectUri = redirectUri
              , token = OAuth.tokenFromString (Maybe.withDefault "" sharedState.token)
              , config = model.config
              }
            , Ports.removeToken ()
            )


configuration : SharedState.SharedState -> Configuration
configuration sharedState =
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
        sharedState.clientId
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
            { clientId = model.config.clientId
            , redirectUri = model.redirectUri
            , scope = model.config.scope
            , state = Just state
            , url = model.config.authorizationEndpoint
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
    case model.flow of
        Types.Done _ ->
            ( model
            , Cmd.none
            )

        _ ->
            ( { model | flow = Types.Authorized token }
            , getUserInfo model.config token
            )


gotUserInfo : SharedState.SharedState -> Model -> Result Http.Error Types.UserInfo -> ( Model, Cmd Msg )
gotUserInfo sharedState model userInfoResponse =
    case userInfoResponse of
        Err _ ->
            ( { model | flow = Types.Errored Types.ErrHTTPGetUserInfo }
            , Ports.removeToken ()
            )

        Ok userInfo ->
            let
                newUrl =
                    sharedState.url
            in
            ( { model | flow = Types.Done userInfo }
            , Navigation.pushUrl sharedState.navKey (Url.toString newUrl)
            )


signOutRequested : Model -> ( Model, Cmd Msg )
signOutRequested model =
    ( { model | flow = Types.Idle }
    , Cmd.batch [ Ports.removeToken (), Navigation.load (Url.toString model.redirectUri) ]
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
        Types.Done userInfo ->
            UserDetails.view userInfo |> fromUnstyled

        Types.Idle ->
            viewIdle

        Types.Authorized _ ->
            viewAuthorized

        Types.Errored err ->
            div []
                [ Grid.row [] [ Grid.col [] [ viewErrored err |> toUnstyled ] ] |> fromUnstyled
                , Grid.row [] [ Grid.col [] [ form |> toUnstyled ] ] |> fromUnstyled
                ]


viewIdle : Html Msg
viewIdle =
    form


viewAuthorized : Html Msg
viewAuthorized =
    Alert.simpleWarning [] [ text "Authorizing user..." |> toUnstyled ] |> fromUnstyled


viewErrored : Types.Error -> Html Msg
viewErrored error =
    Alert.simpleDanger [] [ viewError error |> toUnstyled ] |> fromUnstyled


viewError : Types.Error -> Html Msg
viewError e =
    text <|
        case e of
            Types.ErrStateMismatch ->
                "'state' doesn't match, the request has likely been forged by an adversary!"

            Types.ErrAuthorization error ->
                oauthErrorToString { error = error.error, errorDescription = error.errorDescription }

            Types.ErrHTTPGetUserInfo ->
                "Unable to retrieve user info. Expired OAuth token."


noOp : Model -> ( Model, Cmd Msg )
noOp model =
    ( model, Cmd.none )


update : (Msg -> msg) -> SharedState.SharedState -> Msg -> Model -> ( Model, Cmd msg )
update wrapper sharedState msg model =
    (\( m, ms ) -> ( m, Cmd.map wrapper ms )) <|
        case ( model.flow, msg ) of
            ( _, SignInRequested ) ->
                signInRequested model

            ( Types.Idle, GotRandomBytes bytes ) ->
                gotRandomBytes model bytes

            ( Types.Authorized token, UserInfoRequested ) ->
                userInfoRequested model token

            ( Types.Authorized _, GotUserInfo userInfoResponse ) ->
                gotUserInfo sharedState model userInfoResponse

            ( Types.Done _, SignOutRequested ) ->
                signOutRequested model

            _ ->
                noOp model


authorizedMsg : msg -> Model -> Result String msg
authorizedMsg msg model =
    case model.flow of
        Types.Done _ ->
            Ok msg

        _ ->
            Err "Not authorized"


form : Html Msg
form =
    div []
        [ -- div [ class "background" ]
          --[ div [ class "shape" ] []
          -- , div [ class "shape" ] []
          -- ]
          -- ,
          div [ class "login-form" ]
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
