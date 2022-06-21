module Main exposing (Msg(..), main)

import Auth as Auth exposing (Msg(..))
import Browser exposing (Document, application)
import Browser.Navigation as Navigation
import Decoders as Decoders
import Delay exposing (TimeUnit(..))
import Flags as Flags
import Html as Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (..)
import Ports exposing (..)
import Routing.Route as Route exposing (Msg(..))
import SharedState as SharedState
import Types as Types
import Url exposing (Protocol(..), Url)


type alias Model =
    { url : Url.Url
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
    | NewCoordinates String
    | FullscreenMode Bool


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ randomBytes NewRandomBytes
        , newCoordinatesReceived NewCoordinates
        , fullscreenActive FullscreenMode
        ]


init : Flags.Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags origin navigationKey =
    let
        initSharedState =
            { navKey = navigationKey
            , token = flags.token
            , state = flags.state
            , url = origin
            , viewState = SharedState.Normal
            , clientId = flags.clientId
            , apiOrigin = flags.apiOrigin
            }

        ( routeModel, routeMsg ) =
            Route.init initSharedState origin
    in
    ( { url = origin
      , appState = Ready initSharedState routeModel
      , key = navigationKey
      }
    , Cmd.batch
        [ Cmd.map RouterMsg routeMsg
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.appState of
        Ready sharedState routeModel ->
            let
                wrapped =
                    messageWrapper msg
            in
            case msg of
                ChangedUrl url ->
                    updateRouter { model | url = url } wrapped

                RouterMsg _ ->
                    updateRouter model wrapped

                AuthMessage _ ->
                    updateRouter model wrapped

                NewRandomBytes _ ->
                    updateRouter model wrapped

                NewCoordinates _ ->
                    updateRouter model wrapped

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

                FullscreenMode mode ->
                    ( { model | appState = Ready (SharedState.update sharedState <| SharedState.FullscreenMode mode) routeModel }, Cmd.none )

                NoOp ->
                    noOp model

        NotReady ->
            ( model
            , Cmd.none
            )

        FailedToInitialize ->
            ( model
            , Cmd.none
            )


messageWrapper : Msg -> Route.Msg
messageWrapper msg =
    case msg of
        ChangedUrl url ->
            Route.ChangedUrl url

        RouterMsg routerMsg ->
            routerMsg

        AuthMessage amsg ->
            Route.AuthMsg amsg

        NewRandomBytes bytes ->
            Route.AuthMsg (Auth.GotRandomBytes bytes)

        NewCoordinates scoords ->
            let
                rcoords =
                    Decode.decodeString Decoders.decodeCoordinates scoords
            in
            case rcoords of
                Ok coord ->
                    coord
                        |> Route.routeNewCoordinates

                -- ignore invalid message
                Err _ ->
                    Route.NoOp

        ClickedLink _ ->
            Route.NoOp

        FullscreenMode _ ->
            Route.NoOp

        NoOp ->
            Route.NoOp


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
view { title } model =
    case model.appState of
        Ready sharedState routerModel ->
            Route.view RouterMsg sharedState routerModel

        NotReady ->
            { title = title
            , body = [ Html.div [] [ Html.text "initializing" ] ]
            }

        FailedToInitialize ->
            { title = title
            , body = [ Html.div [] [ Html.text "Initialization failed" ] ]
            }


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
