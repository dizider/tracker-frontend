module Routing.Route exposing (..)

import Browser
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (href)
import Html.Styled.Events exposing (..)
import Pages.Home as Home
import Pages.Trackers as Trackers
import SharedState as SharedState
import Url exposing (Url)
import Url.Parser as Parser exposing (..)
import Auth


type Tracker
    = Tracker Int


type Route
    = NotFound
      -- | AccessDenied
    | TrackersList
    | HomePage



-- | Map Tracker
-- | Analyze Tracker


type Msg
    = ChangedUrl Url.Url
    | NavigateTo Route
    | TrackersMsg Trackers.Msg
    | HomeMsg Home.Msg


type alias Model =
    { route : Route
    , trackersListModel : Trackers.Model
    , homeModel : Home.Model
    }


init : Url -> ( Model, Cmd Msg )
init url =
    ( { trackersListModel = Trackers.initModel
      , homeModel = Home.initModel
      , route = parseUrl url
      }
    , Cmd.map HomeMsg Cmd.none
    )


update : SharedState.SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
update sharedState msg model =
    case msg of
        ChangedUrl location ->
            ( { model | route = parseUrl location }
            , Cmd.none
            , SharedState.NoUpdate
            )

        NavigateTo route ->
            ( model
            , Browser.Navigation.pushUrl sharedState.navKey (routeToString route)
            , SharedState.NoUpdate
            )

        TrackersMsg tmsg ->
            updateTrackers sharedState model tmsg

        HomeMsg hmsg ->
            updateHome sharedState model hmsg


updateTrackers : SharedState.SharedState -> Model -> Trackers.Msg -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
updateTrackers sharedState model trackersMsg =
    let
        ( nextHomeModel, trackersCmd, trackerSharedState ) =
            Trackers.update sharedState trackersMsg model.trackersListModel
    in
    ( { model | trackersListModel = nextHomeModel }
    , Cmd.map TrackersMsg trackersCmd
    , trackerSharedState
    )


updateHome : SharedState.SharedState -> Model -> Home.Msg -> ( Model, Cmd Msg, SharedState.SharedStateUpdate )
updateHome sharedState model trackersMsg =
    ( model, Cmd.none, SharedState.NoUpdate )

view : (Msg -> msg) -> SharedState.SharedState -> Model -> Browser.Document msg
view msgMapper sharedState model =
    let
        title =
            case model.route of
                HomePage ->
                    "Home"

                TrackersList ->
                    "Tracker List"

                NotFound ->
                    "NotFound"

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
                        [ onClick (NavigateTo TrackersList)
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

            TrackersList ->
                Trackers.view sharedState model.trackersListModel
                    |> Html.Styled.map TrackersMsg

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
        [ -- Parser.map Map <| Parser.s "map" </> Parser.map Tracker Parser.int
          Parser.map HomePage (Parser.s "home")
        , Parser.map TrackersList (Parser.s "trackers")
        ]


routeToString : Route -> String
routeToString route =
    case route of
        -- Map tracker ->
        --     "map"
        -- Analyze tracker ->
        --     "analyze"
        NotFound ->
            "/not-found"

        -- AccessDenied ->
        --     "access-denied"
        TrackersList ->
            "/trackers"

        HomePage ->
            "/home"
