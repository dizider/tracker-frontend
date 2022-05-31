module Routing.Route exposing (..)

import Auth
import Browser
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (href)
import Html.Styled.Events exposing (..)
import Pages.Home as Home
import Pages.Map as Map
import Pages.Tracks as Tracks
import SharedState as SharedState
import Types as Types
import Url exposing (Url)
import Url.Parser as Parser exposing (..)


type Tracker
    = Tracker Int


type Route
    = NotFound
    | TracksList
    | HomePage
    | AuthPage
    | MapPage


type Msg
    = ChangedUrl Url.Url
    | NavigateTo Route
    | TracksMsg Tracks.Msg
    | HomeMsg Home.Msg
    | AuthMsg Auth.Msg
    | MapMsg Map.Msg


type alias Model =
    { route : Route
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
      , authModel = authModel
      , mapModel = mapModel
      }
    , Cmd.batch
        [ Cmd.map HomeMsg Cmd.none
        , Cmd.map TracksMsg trackersMsg
        , Cmd.map AuthMsg authMsg
        , Cmd.map MapMsg mapMsg
        ]
    )


update : SharedState.SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
update sharedState msg model =
    case msg of
        AuthMsg amsg ->
            let
                _ =
                    Debug.log "Route AuthMsg" sharedState
            in
            updateAuth sharedState model amsg

        ChangedUrl location ->
            ( { model | route = parseUrl location }
            , Cmd.none
            , SharedState.NoUpdate
            )

        NavigateTo route ->
            case route of
                TracksList ->
                    ( model
                    , Cmd.batch [ Cmd.map TracksMsg Tracks.fetchData, Browser.Navigation.pushUrl sharedState.navKey (routeToString route) ]
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
            updateHome sharedState model hmsg

        MapMsg mmsg ->
            updateMap sharedState model mmsg


updateAuth : SharedState.SharedState -> Model -> Auth.Msg -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
updateAuth _ model authMsg =
    let
        ( newModel, authCmd ) =
            Auth.update authMsg model.authModel
    in
    ( { model | authModel = newModel }
    , Cmd.map AuthMsg authCmd
    , SharedState.NoUpdate
    )


updateTrackers : SharedState.SharedState -> Model -> Tracks.Msg -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
updateTrackers sharedState model trackersMsg =
    let
        _ =
            Debug.log "updateTrackers" ""

        ( nextHomeModel, trackersCmd, trackerSharedState ) =
            Tracks.update sharedState trackersMsg model.tracksListModel
    in
    ( { model | tracksListModel = nextHomeModel }
    , Cmd.map TracksMsg trackersCmd
    , trackerSharedState
    )


updateHome : SharedState.SharedState -> Model -> Home.Msg -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
updateHome sharedState model trackersMsg =
    ( model, Cmd.none, SharedState.NoUpdate )


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

                MapPage ->
                    "Map"

        body =
            div []
                [ header []
                    [ h1 [] [ text "site-title" ]
                    ]
                , nav []
                    [ button
                        [ onClick (NavigateTo HomePage)
                        ]
                        [ text "page-title-home" ]
                    , button
                        [ onClick (NavigateTo TracksList)
                        ]
                        [ text "page-title-settings" ]
                    ]
                , pageView sharedState model
                , footer []
                    [ text ("footer-github-before" ++ " ")
                    , a
                        [ href "https://github.com/ohanhi/elm-shared-state/"
                        ]
                        [ text "Github" ]
                    , text "footer-github-after"
                    ]
                ]
    in
    { title = title ++ " - Elm Shared State Demo"
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
                        Html.Styled.text userInfo.email

                    _ ->
                        Html.Styled.map AuthMsg Auth.viewIdle

            MapPage ->
                Map.view sharedState model.mapModel
                    |> Html.Styled.map MapMsg

            NotFound ->
                h1 [] [ text "404 :(" ]
        ]


parseUrl : Url -> Route
parseUrl url =
    case Parser.parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


matchRoute : Parser.Parser (Route -> a) a
matchRoute =
    Parser.oneOf
        [ --Parser.map MapPage <| Parser.s "map" </> Parser.map Tracker Parser.int
          Parser.map HomePage (Parser.s "home")
        , Parser.map HomePage Parser.top
        , Parser.map TracksList (Parser.s "trackers")
        , Parser.map AuthPage (Parser.s "auth")
        , Parser.map AuthPage (Parser.s "map")
        ]


routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            "/not-found"

        TracksList ->
            "/trackers"

        HomePage ->
            "/home"

        AuthPage ->
            "/auth"

        MapPage ->
            "/map"
