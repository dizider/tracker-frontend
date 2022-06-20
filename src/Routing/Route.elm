module Routing.Route exposing (Model, Msg(..), init, routeNewCoordinates, update, view)

import Auth
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Browser.Navigation
import Html exposing (..)
import Html.Attributes as Attributes exposing (href)
import Html.Events exposing (..)
import Html.Styled
import OAuth exposing (ErrorCode(..))
import Pages.Home as Home
import Pages.Map as Map
import Pages.Trackers as Trackers
import Pages.Tracks as Tracks
import Routing.Helpers as Helpers exposing (..)
import SharedState as SharedState
import Types as Types
import Url exposing (Url)


type Msg
    = ChangedUrl Url.Url
    | NewCoordinates Types.Coordinates
    | NavigateTo Route
    | NavbarMsg Navbar.State
    | TracksMsg Tracks.Msg
    | TrackersMsg Trackers.Msg
    | HomeMsg Home.Msg
    | AuthMsg Auth.Msg
    | MapMsg Map.Msg
    | AuthorizedMsg (Cmd Msg)
    | AccessDenied
    | NoOp


type alias Model =
    { route : Route
    , url : Url
    , navbarState : Navbar.State
    , tracksListModel : Tracks.Model
    , trackersModel : Trackers.Model
    , homeModel : Home.Model
    , authModel : Auth.Model
    , mapModel : Map.Model
    }


init : SharedState.SharedState -> Url -> ( Model, Cmd Msg )
init sharedState url =
    let
        tracksModel =
            Tracks.initModel

        trackersModel =
            Trackers.initModel

        ( authModel, authMsg ) =
            Auth.init sharedState url sharedState.navKey

        ( mapModel, mapMsg ) =
            Map.init []

        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { tracksListModel = tracksModel
      , trackersModel = trackersModel
      , homeModel = Home.initModel
      , route = parseUrl url
      , url = url
      , authModel = authModel
      , mapModel = mapModel
      , navbarState = navbarState
      }
    , Cmd.batch
        [ Cmd.map HomeMsg Cmd.none
        , Cmd.map AuthMsg authMsg
        , Cmd.map MapMsg mapMsg
        , navbarCmd

        -- , updatePage url
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
            let
                ( newModel, newMsg ) =
                    loadPage location model
            in
            ( { newModel | route = parseUrl location, url = location }
            , newMsg
            , SharedState.NoUpdate
            )

        NavigateTo route ->
            case route of
                Tracks ->
                    ( model
                    , Cmd.batch [ Cmd.map TracksMsg Tracks.fetchData, Browser.Navigation.pushUrl sharedState.navKey (routeToString route) ]
                    , SharedState.NoUpdate
                    )

                MapPage mtrackId ->
                    case mtrackId of
                        Just ids ->
                            let
                                ( updateMapModel, mapMsg ) =
                                    Map.fetchData ids model.mapModel
                            in
                            ( { model | mapModel = updateMapModel }
                            , Cmd.batch [ Cmd.map MapMsg mapMsg, Browser.Navigation.pushUrl sharedState.navKey (routeToString route) ]
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
            updateTracks sharedState model tmsg

        TrackersMsg tmsg ->
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
                            Home.update HomeMsg sharedState (Home.NewCoordinates coords) model.homeModel
                    in
                    ( { model | homeModel = updatedHomeModel }
                    , updatedHomeMsg
                    , updatedSharedState
                    )

                -- TODO : appending new positon to track
                MapPage _ ->
                    ( model, Cmd.none, SharedState.NoUpdate )

                _ ->
                    ( model, Cmd.none, SharedState.NoUpdate )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none, SharedState.NoUpdate )

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
    in
    case ( resultMsg, model.route ) of
        ( _, AuthPage ) ->
            case msg of
                AuthMsg _ ->
                    authUpdateProxy sharedState msg model

                _ ->
                    case resultMsg of
                        Ok _ ->
                            let
                                updateModel =
                                    { model | route = parseUrl model.url }
                            in
                            authUpdateProxy sharedState msg updateModel

                        Err _ ->
                            authUpdateProxy sharedState AccessDenied model

        ( Ok _, _ ) ->
            let
                updateModel =
                    { model | route = parseUrl model.url }
            in
            authUpdateProxy sharedState msg updateModel

        ( Err _, _ ) ->
            authUpdateProxy sharedState AccessDenied model


loadPage : Url -> Model -> ( Model, Cmd Msg )
loadPage url model =
    case parseUrl url of
        MapPage mtrackId ->
            case mtrackId of
                Just ids ->
                    let
                        ( newModel, newMsg ) =
                            Map.fetchData ids model.mapModel
                    in
                    ( { model | mapModel = newModel }, Cmd.map MapMsg newMsg )

                Nothing ->
                    ( model, Cmd.none )

        NotFound ->
            ( model, Cmd.none )

        Tracks ->
            ( model, Cmd.map TracksMsg Tracks.fetchData )

        Trackers ->
            ( model, Cmd.map TrackersMsg Trackers.fetchData )

        HomePage ->
            ( model, Home.load )

        AuthPage ->
            ( model, Cmd.none )

        AccessDeniedPage ->
            ( model, Cmd.none )


updateAuth : SharedState.SharedState -> Model -> Auth.Msg -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
updateAuth sharedState model authMsg =
    let
        ( newModel, authCmd ) =
            Auth.update AuthMsg sharedState authMsg model.authModel
    in
    ( { model | authModel = newModel }
    , authCmd
    , SharedState.NoUpdate
    )


updateTracks : SharedState.SharedState -> Model -> Tracks.Msg -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
updateTracks sharedState model tracksMsg =
    let
        ( nextHomeModel, tracksCmd, tracksSharedState ) =
            Tracks.update TracksMsg sharedState tracksMsg model.tracksListModel
    in
    ( { model | tracksListModel = nextHomeModel }
    , tracksCmd
    , tracksSharedState
    )


updateTrackers : SharedState.SharedState -> Model -> Trackers.Msg -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
updateTrackers sharedState model tracksMsg =
    let
        ( nextHomeModel, trackersCmd, trackersSharedState ) =
            Trackers.update TrackersMsg sharedState tracksMsg model.trackersModel
    in
    ( { model | trackersModel = nextHomeModel }
    , trackersCmd
    , trackersSharedState
    )


updateHome : SharedState.SharedState -> Model -> Home.Msg -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
updateHome sharedState model homeMsg =
    let
        ( newModel, homeCmd, updatedSharedState ) =
            Home.update HomeMsg sharedState homeMsg model.homeModel
    in
    ( { model | homeModel = newModel }
    , homeCmd
    , updatedSharedState
    )


updateMap : SharedState.SharedState -> Model -> Map.Msg -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
updateMap sharedState model mapMsg =
    let
        ( newModel, mapCmd, sharedStateUpdate ) =
            Map.update MapMsg mapMsg model.mapModel sharedState
    in
    ( { model | mapModel = newModel }
    , mapCmd
    , sharedStateUpdate
    )


view : (Msg -> msg) -> SharedState.SharedState -> Model -> Browser.Document msg
view msgMapper sharedState model =
    let
        title =
            Helpers.routeToTitle model.route

        body =
            case ( model.route, model.authModel.flow ) of
                ( AuthPage, Types.Authorized _ ) ->
                    pageView sharedState model

                ( AuthPage, Types.Idle ) ->
                    pageView sharedState model

                ( AuthPage, Types.Errored _ ) ->
                    pageView sharedState model

                ( AccessDeniedPage, _ ) ->
                    pageView sharedState model

                _ ->
                    div []
                        [ Navbar.config NavbarMsg
                            |> Navbar.brand [ href "#" ] [ text "Trackers" ]
                            |> Navbar.items (List.map (\route -> navigationLinkView (model.route == route) route) linkList)
                            |> Navbar.customItems (navigationCustomView model)
                            |> Navbar.view model.navbarState
                        , pageView sharedState model
                        , footer [ Attributes.class "footer", Spacing.mtAuto, Spacing.py3 ]
                            [ Grid.container
                                []
                                [ a [ Attributes.href "https://github.com/litvinov-tabor2022" ] [ text "Github" ] ]
                            ]
                        ]
    in
    { title = title ++ " - Tracker"
    , body =
        [ Grid.container []
            [ CDN.stylesheet ]
        , body
            |> Html.map msgMapper
        ]
    }


navigationCustomView : Model -> List (Navbar.CustomItem Msg)
navigationCustomView model =
    case model.authModel.flow of
        Types.Done userInfo ->
            [ Navbar.textItem [ Spacing.px3 ] [ text userInfo.name ]
            , Navbar.formItem [] [ Button.button [ Button.small, Button.outlineWarning, Button.onClick (AuthMsg Auth.SignOutRequested) ] [ text "Logout" ] ]
            ]

        _ ->
            [ Navbar.customItem <| Html.div [] [] ]


navigationLinkView : Bool -> Helpers.Route -> Navbar.Item msg
navigationLinkView isActive route =
    let
        title =
            Helpers.routeToTitle route |> text
    in
    if isActive then
        Navbar.itemLinkActive [ Helpers.routeToString route |> href ] [ title ]

    else
        Navbar.itemLink [ Helpers.routeToString route |> href ] [ title ]


linkList : List Helpers.Route
linkList =
    [ Helpers.HomePage
    , Helpers.Tracks
    , Helpers.Trackers
    ]


pageView : SharedState.SharedState -> Model -> Html Msg
pageView sharedState model =
    template <|
        case model.route of
            HomePage ->
                Grid.col [ Col.attrs [ Spacing.p0, Flex.alignSelfCenter ] ]
                    [ Home.view sharedState model.homeModel
                        |> Html.map HomeMsg
                    ]

            Tracks ->
                Grid.col [ Col.lg8, Col.sm12, Col.attrs [ Spacing.py5, Spacing.p0, Flex.alignSelfCenter ] ]
                    [ Tracks.view sharedState model.tracksListModel
                        |> Html.map TracksMsg
                    ]

            Trackers ->
                Grid.col [ Col.lg8, Col.sm12, Col.attrs [ Spacing.py5, Spacing.p0, Flex.alignSelfCenter ] ]
                    [ Trackers.view model.trackersModel
                        |> Html.map TrackersMsg
                    ]

            AuthPage ->
                Grid.col [ Col.lg12, Col.sm12, Col.md12, Col.attrs [ Spacing.py5, Flex.block, Spacing.p0, Flex.alignSelfCenter, Flex.justifyCenter ] ]
                    [ Auth.view model.authModel
                        |> Html.Styled.toUnstyled
                        |> Html.map AuthMsg
                    ]

            MapPage _ ->
                Grid.col [ Col.attrs [ Spacing.p0, Flex.alignSelfCenter ] ]
                    [ Map.view sharedState model.mapModel
                        |> Html.Styled.toUnstyled
                        |> Html.map MapMsg
                    ]

            NotFound ->
                Grid.col [] [ h1 [] [ text "404 :(" ] ]

            AccessDeniedPage ->
                Grid.col [] [ h1 [] [ text "Access denied" ] ]


template : Grid.Column msg -> Html msg
template content =
    Grid.containerFluid []
        [ Grid.row [ Row.attrs [ Flex.block, Flex.justifyCenter ] ]
            [ content
            ]
        ]


routeNewCoordinates : Types.Coordinates -> Msg
routeNewCoordinates coords =
    coords
        |> NewCoordinates
