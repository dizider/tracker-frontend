module Main exposing (Msg(..), liftToMain, main)

import Auth as Auth exposing (Msg(..), viewErrored)
import Browser exposing (Document, application)
import Browser.Navigation as Navigation exposing (Key)
import Bytes exposing (Bytes)
import Decoders as Decoders
import Delay exposing (TimeUnit(..))
import Flags as Flags
import Html as Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events as Events exposing (..)
import Json.Decode as Decode exposing (..)
import Ports exposing (..)
import Routing.Route as Route exposing (Msg(..))
import SharedState as SharedState
import Types as Types
import Url exposing (Protocol(..), Url)


type alias Model =
    { auth : Auth.Model
    , url : Url.Url
    , appState : AppState
    , key : Navigation.Key
    }


type AppState
    = NotReady
    | Ready SharedState.SharedState Route.Model
    | FailedToInitialize


type Msg
    = NoOp
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | RouterMsg Route.Msg
    | AuthMessage Auth.Msg
    | NewRandomBytes (List Int)
    | NewPersistedToken String
    | NewCoordinates String


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ randomBytes NewRandomBytes
        , persistedToken NewPersistedToken
        , newCoordinatesReceived NewCoordinates
        ]


init : Flags.Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags origin navigationKey =
    let
        initSharedState =
            { navKey = navigationKey
            , token = flags.token
            , state = flags.state
            , url = origin
            }

        ( routeModel, routeMsg ) =
            Route.init initSharedState origin

        -- ( authModel, authMsg ) =
        --     Auth.init initSharedState origin navigationKey
        authModel =
            Auth.initModel initSharedState origin flags.token
    in
    ( { auth = authModel
      , url = origin
      , appState = Ready initSharedState routeModel
      , key = navigationKey
      }
    , Cmd.batch
        [ Cmd.map RouterMsg routeMsg
        , Cmd.map (always ChangedUrl origin) Cmd.none

        -- , Cmd.map (always Auth.ini)
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.appState of
        Ready sharedState r ->
            case msg of
                ChangedUrl url ->
                    updateRouter { model | url = url } (Route.ChangedUrl url)

                RouterMsg routerMsg ->
                    updateRouter model routerMsg

                AuthMessage amsg ->
                    let
                        updatedAuth =
                            Auth.update sharedState amsg model.auth
                    in
                    ( { model | auth = Tuple.first updatedAuth }
                    , Cmd.map liftToMain (Tuple.second updatedAuth)
                    )

                NewRandomBytes bytes ->
                    let
                        updatedAuth =
                            Auth.update sharedState (Auth.GotRandomBytes bytes) model.auth
                    in
                    ( { model | auth = Tuple.first updatedAuth }
                    , Cmd.map liftToMain (Tuple.second updatedAuth)
                    )

                NewPersistedToken token ->
                    ( model
                    , Cmd.map
                        (token
                            |> GotPersistedToken
                            |> always liftToMain
                        )
                        Cmd.none
                    )

                ClickedLink urlRequest ->
                    ( model
                    , case urlRequest of
                        Browser.Internal url ->
                            url
                                |> Url.toString
                                |> Navigation.pushUrl model.key

                        Browser.External href ->
                            Navigation.load href
                    )

                NewCoordinates scoords ->
                    let
                        rcoords =
                            Decode.decodeString Decoders.decodeCoordinates scoords
                    in
                    case rcoords of
                        Ok coord ->
                            coord
                                |> Route.routeNewCoordinates
                                |> updateRouter model

                        -- ignore invalid message
                        Err _ ->
                            ( model
                            , Cmd.none
                            )

                _ ->
                    noOp model

        _ ->
            ( model
            , Cmd.none
            )


updateRouter : Model -> Route.Msg -> ( Model, Cmd Msg )
updateRouter model routerMsg =
    case model.appState of
        Ready sharedState routerModel ->
            let
                nextSharedState =
                    SharedState.update sharedState sharedStateUpdate

                ( nextRouterModel, routerCmd, sharedStateUpdate ) =
                    Route.update sharedState routerMsg routerModel
            in
            ( { model | appState = Ready nextSharedState nextRouterModel }
            , Cmd.map RouterMsg routerCmd
            )

        _ ->
            ( model, Cmd.none )


noOp : Model -> ( Model, Cmd Msg )
noOp model =
    ( model, Cmd.none )


view : Types.ViewConfiguration -> Model -> Document Msg
view ({ title } as config) model =
    let
        bootstrap =
            []
    in
    case model.appState of
        -- Types.Done user ->
        --     { title = title
        --     , body = bootstrap ++ [ mapView [] ]
        --     }
        Ready sharedState routerModel ->
            Route.view RouterMsg sharedState routerModel

        NotReady ->
            { title = title
            , body = [ Html.div [] [ Html.text "initializing" ] ]
            }

        _ ->
            { title = title
            , body = [ Html.div [] [ Html.text "nothing here" ] ]
            }





viewUserInfo : Types.ViewConfiguration -> Types.UserInfo -> List (Html.Html Msg)
viewUserInfo _ { name, picture, email } =
    [ Html.div [ class "flex", class "flex-column" ]
        [ Html.img [ class "avatar", Attributes.src picture ] []
        , Html.p [] [ Html.text name ]
        , Html.p [] [ Html.text email ]
        , Html.div []
            [ Html.button
                [ Events.onClick (liftToMain Auth.SignOutRequested) ]
                [ Html.text "Sign out" ]
            ]
        ]
    ]


viewAuthorizationStep : Bool -> Html.Html Msg
viewAuthorizationStep isActive =
    viewStep isActive ( "Authorization", style "left" "-110%" )


viewGetUserInfoStep : Bool -> Html.Html Msg
viewGetUserInfoStep isActive =
    viewStep isActive ( "Get User Info", style "left" "-135%" )


viewErroredStep : Html.Html Msg
viewErroredStep =
    Html.div
        [ class "step", class "step-errored" ]
        [ Html.span [ style "left" "-50%" ] [ Html.h1 [] [ Html.text "Authorization error" ] ] ]


viewStep : Bool -> ( String, Html.Attribute Msg ) -> Html.Html Msg
viewStep isActive ( step, position ) =
    let
        stepClass =
            class "step"
                :: (if isActive then
                        [ class "step-active" ]

                    else
                        []
                   )
    in
    Html.div stepClass [ Html.span [ position ] [ Html.text step ] ]


viewStepSeparator : Bool -> Html.Html Msg
viewStepSeparator isActive =
    let
        stepClass =
            class "step-separator"
                :: (if isActive then
                        [ class "step-active" ]

                    else
                        []
                   )
    in
    Html.span stepClass []


liftToMain : Auth.Msg -> Msg
liftToMain msg =
    AuthMessage msg


main : Program Flags.RawFlags Model Msg
main =
    application
        { init =
            Flags.decodeFlags >> init
        , update =
            update
        , subscriptions =
            subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , view =
            view
                { title = "Tracker"
                }
        }
