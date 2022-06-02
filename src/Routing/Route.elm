module Routing.Route exposing (Model, Msg(..), init, routeNewCoordinates, update, view)

import Auth
import Browser
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (href)
import Html.Styled.Events exposing (..)
import OAuth exposing (ErrorCode(..))
import Pages.Home as Home
import Pages.Map as Map
import Pages.Tracks as Tracks
import Routing.Helpers as Helpers exposing (..)
import SharedState as SharedState
import Types as Types
import Url exposing (Url)
import Url.Parser as Parser exposing (..)


type Msg
    = ChangedUrl Url.Url
    | NewCoordinates Types.Coordinates
    | NavigateTo Route
    | TracksMsg Tracks.Msg
    | HomeMsg Home.Msg
    | AuthMsg Auth.Msg
    | MapMsg Map.Msg
    | AuthorizedMsg (Cmd Msg)
    | AccessDenied
    | NoOp


type alias Model =
    { route : Route
    , url : Url
    , tracksListModel : Tracks.Model
    , homeModel : Home.Model
    , authModel : Auth.Model
    , mapModel : Map.Model
    }


init : SharedState.SharedState -> Url -> ( Model, Cmd Msg )
init sharedState url =
    let
        ( tracksModel, trackersMsg ) =
            Tracks.init

        ( authModel, authMsg ) =
            Auth.init sharedState url sharedState.navKey

        ( mapModel, mapMsg ) =
            Map.init []
    in
    ( { tracksListModel = tracksModel
      , homeModel = Home.initModel
      , route = parseUrl url
      , url = url
      , authModel = authModel
      , mapModel = mapModel
      }
    , Cmd.batch
        [ Cmd.map HomeMsg Cmd.none
        , Cmd.map TracksMsg trackersMsg
        , Cmd.map AuthMsg authMsg
        , Cmd.map MapMsg mapMsg
        , updatePage url
        ]
    )


authUpdateProxy : SharedState.SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
authUpdateProxy sharedState msg model =
    case msg of
        AccessDenied ->
            ( { model | route = AuthPage }
            , Cmd.none
            , SharedState.UpdateToken Nothing
            )

        AuthorizedMsg authMsg ->
            let
                resultMsg =
                    Auth.authorizedMsg authMsg model.authModel
            in
            case resultMsg of
                Ok authorizedMsg ->
                    ( model
                    , authorizedMsg
                    , SharedState.NoUpdate
                    )

                Err _ ->
                    ( { model | route = AuthPage }
                    , Cmd.map (\_ -> AccessDenied) Cmd.none
                    , SharedState.NoUpdate
                    )

        AuthMsg amsg ->
            updateAuth sharedState model amsg

        ChangedUrl location ->
            ( { model | route = parseUrl location, url = location }
            , updatePage location
            , SharedState.NoUpdate
            )

        NavigateTo route ->
            case route of
                TracksList ->
                    ( model
                    , Cmd.batch [ Cmd.map TracksMsg Tracks.fetchData, Browser.Navigation.pushUrl sharedState.navKey (routeToString route) ]
                    , SharedState.NoUpdate
                    )

                MapPage mtrackId ->
                    case mtrackId of
                        Just id ->
                            ( model
                            , Cmd.batch [ Cmd.map MapMsg (Map.fetchData [ id ]), Browser.Navigation.pushUrl sharedState.navKey (routeToString route) ]
                            , SharedState.NoUpdate
                            )

                        Nothing ->
                            ( model
                            , Browser.Navigation.pushUrl sharedState.navKey (routeToString route)
                            , SharedState.NoUpdate
                            )

                _ ->
                    ( model
                    , Browser.Navigation.pushUrl sharedState.navKey (routeToString route)
                    , SharedState.NoUpdate
                    )

        TracksMsg tmsg ->
            updateTrackers sharedState model tmsg

        HomeMsg hmsg ->
            -- TODO: fix handling new coords
            updateHome sharedState model hmsg

        MapMsg mmsg ->
            updateMap sharedState model mmsg

        NewCoordinates coords ->
            case model.route of
                HomePage ->
                    let
                        ( updatedHomeModel, updatedHomeMsg, updatedSharedState ) =
                            Home.update sharedState (Home.NewCoordinates coords) model.homeModel
                    in
                    ( { model | homeModel = updatedHomeModel }
                    , Cmd.map HomeMsg updatedHomeMsg
                    , updatedSharedState
                    )

                -- TODO : appending new positon to track
                MapPage _ ->
                    ( model, Cmd.none, SharedState.NoUpdate )

                _ ->
                    ( model, Cmd.none, SharedState.NoUpdate )

        NoOp ->
            ( model
            , Cmd.none
            , SharedState.NoUpdate
            )


update : SharedState.SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
update sharedState msg model =
    let
        resultMsg =
            Auth.authorizedMsg msg model.authModel

        accessDeniedModel =
            { model | route = AuthPage }
    in
    case ( resultMsg, model.route ) of
        ( _, AuthPage ) ->
            case msg of
                (AuthMsg _) as amsg ->
                    authUpdateProxy sharedState msg model

                _ ->
                    case resultMsg of
                        Ok authMsg ->
                            let
                                updateModel =
                                    { model | route = parseUrl model.url }
                            in
                            authUpdateProxy sharedState msg updateModel

                        Err _ ->
                            authUpdateProxy sharedState AccessDenied model

        ( Ok authMsg, _ ) ->
            let
                updateModel =
                    { model | route = parseUrl model.url }
            in
            authUpdateProxy sharedState msg updateModel

        ( Err _, _ ) ->
            authUpdateProxy sharedState AccessDenied model


updatePage : Url -> Cmd Msg
updatePage url =
    case parseUrl url of
        MapPage mtrackId ->
            case mtrackId of
                Just id ->
                    Cmd.map MapMsg (Map.fetchData [ id ])

                Nothing ->
                    Cmd.none

        NotFound ->
            Cmd.none

        TracksList ->
            Cmd.map TracksMsg Tracks.fetchData

        HomePage ->
            Cmd.none

        AuthPage ->
            Cmd.none


updateAuth : SharedState.SharedState -> Model -> Auth.Msg -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
updateAuth sharedState model authMsg =
    let
        ( newModel, authCmd ) =
            Auth.update sharedState authMsg model.authModel
    in
    ( { model | authModel = newModel }
    , Cmd.map AuthMsg authCmd
    , SharedState.NoUpdate
    )


updateTrackers : SharedState.SharedState -> Model -> Tracks.Msg -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
updateTrackers sharedState model trackersMsg =
    let
        -- _ =
        -- Debug.log "updateTrackers" ""
        ( nextHomeModel, trackersCmd, trackerSharedState ) =
            Tracks.update sharedState trackersMsg model.tracksListModel
    in
    ( { model | tracksListModel = nextHomeModel }
    , Cmd.map TracksMsg trackersCmd
    , trackerSharedState
    )


updateHome : SharedState.SharedState -> Model -> Home.Msg -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
updateHome sharedState model homeMsg =
    let
        ( newModel, homeCmd, updatedSharedState ) =
            Home.update sharedState homeMsg model.homeModel
    in
    ( { model | homeModel = newModel }
    , Cmd.map HomeMsg homeCmd
    , updatedSharedState
    )


updateMap : SharedState.SharedState -> Model -> Map.Msg -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
updateMap sharedState model mapMsg =
    let
        ( newModel, mapCmd ) =
            Map.update mapMsg model.mapModel
    in
    ( { model | mapModel = newModel }
    , Cmd.map MapMsg mapCmd
    , SharedState.NoUpdate
    )


view : (Msg -> msg) -> SharedState.SharedState -> Model -> Browser.Document msg
view msgMapper sharedState model =
    let
        title =
            case model.route of
                HomePage ->
                    "Home"

                TracksList ->
                    "Tracker List"

                NotFound ->
                    "NotFound"

                AuthPage ->
                    "Autorization"

                MapPage mtrack ->
                    case mtrack of
                        Nothing ->
                            "Map"

                        Just (TrackId track) ->
                            "Track" ++ String.fromInt track

        body =
            case ( model.route, model.authModel.flow ) of
                ( HomePage, _ ) ->
                    pageView sharedState model

                ( AuthPage, Types.Authorized _ ) ->
                    pageView sharedState model

                ( AuthPage, Types.Idle ) ->
                    pageView sharedState model

                _ ->
                    div []
                        [ header []
                            [ h1 [] [] --text "site-title" ]
                            ]
                        , nav []
                            [ button
                                [ onClick (NavigateTo HomePage)
                                ]
                                [ text "Home" ]
                            , button
                                [ onClick (NavigateTo TracksList)
                                ]
                                [ text "Tracks list" ]
                            ]
                        , pageView sharedState model
                        , footer []
                            [ a
                                [ Attributes.href "https://github.com/litvinov-tabor2022"
                                ]
                                [ text "Github" ]
                            ]
                        ]
    in
    { title = title ++ " - Tracker"
    , body =
        [ body
            |> Html.Styled.toUnstyled
            |> Html.map msgMapper
        ]
    }


pageView : SharedState.SharedState -> Model -> Html Msg
pageView sharedState model =
    div []
        [ case model.route of
            HomePage ->
                Home.view sharedState model.homeModel
                    |> Html.Styled.map HomeMsg

            TracksList ->
                Tracks.view sharedState model.tracksListModel
                    |> Html.Styled.map TracksMsg

            AuthPage ->
                case model.authModel.flow of
                    Types.Done userInfo ->
                        div []
                            [ text "User details: "
                            , ul []
                                [ li [] [ text "email: ", text userInfo.email ]
                                , li [] [ text "name: ", text userInfo.name ]
                                ]
                            ]

                    Types.Idle ->
                        Html.Styled.map AuthMsg Auth.viewIdle

                    Types.Authorized _ ->
                        Html.Styled.map AuthMsg Auth.viewAuthorized

                    Types.Errored err ->
                        Html.Styled.map AuthMsg (Auth.viewErrored err)

            MapPage mtrack ->
                Map.view sharedState model.mapModel
                    |> Html.Styled.map MapMsg

            NotFound ->
                h1 [] [ text "404 :(" ]
        ]


routeNewCoordinates : Types.Coordinates -> Msg
routeNewCoordinates coords =
    coords
        |> NewCoordinates
