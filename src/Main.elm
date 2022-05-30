module Main exposing (main, viewMap)

import Auth as Auth exposing (Msg(..), viewErrored)
import Base64.Encode as Base64
import Browser exposing (Document, application)
import Browser.Navigation exposing (Key)
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import Delay exposing (TimeUnit(..))
import Helpers as Helpers
import Html as Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events as Events exposing (..)
import PortFunnel.WebSocket exposing (Message)
import Ports exposing (persistedToken, randomBytes)
import Types exposing (Model)
import Url exposing (Protocol(..), Url)


main : Program Types.RawFlags Model Msg
main =
    application
        { init =
            processFlags >> init
        , update =
            update
        , subscriptions =
            subscriptions
        , onUrlRequest =
            always NoOp
        , onUrlChange =
            always NoOp
        , view =
            view
                { title = "Tracker"
                , btnClass = class "btn-google"
                }
        }


processFlags : Types.RawFlags -> Types.Flags
processFlags flags =
    let
        state =
            Maybe.map Helpers.convertBytes flags.state
    in
    { state = state
    , clientId = flags.clientId
    , token = flags.token
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ randomBytes NewRandomBytes
        , persistedToken NewPersistedToken
        ]


{-| During the authentication flow, we'll run twice into the `init` function:

  - The first time, for the application very first run. And we proceed with the `Idle` state,
    waiting for the user (a.k.a you) to request a sign in.

  - The second time, after a sign in has been requested, the user is redirected to the
    authorization server and redirects the user back to our application, with an access
    token and other fields as query parameters.

When query params are present (and valid), we consider the user `Authorized`.

-}
init : Types.Flags -> Url -> Key -> ( Model, Cmd Msg )
init mflags origin navigationKey =
    let
        auth =
            Auth.init mflags origin navigationKey
    in
    ( { auth = Tuple.first auth, clientId = "" }
    , Cmd.map (\x -> AuthMessage x) (Tuple.second auth)
    )



--
-- Msg
--


type Msg
    = NoOp
    | AuthMessage Auth.Msg
    | NewRandomBytes (List Int)
    | NewPersistedToken String



{- On the JavaScript's side, we have:

   app.ports.genRandomBytes.subscribe(n => {
     const buffer = new Uint8Array(n);
     crypto.getRandomValues(buffer);
     const bytes = Array.from(buffer);
     localStorage.setItem("bytes", bytes);
     app.ports.randomBytes.send(bytes);
   });
-}
--
-- Update
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthMessage amsg ->
            let
                updatedAuth =
                    Auth.update amsg model.auth
            in
            ( { model | auth = Tuple.first updatedAuth }
            , Cmd.map AuthMessage (Tuple.second updatedAuth)
            )

        NewRandomBytes bytes ->
            let
                updatedAuth =
                    Auth.update (Auth.GotRandomBytes bytes) model.auth
            in
            ( { model | auth = Tuple.first updatedAuth }
            , Cmd.map AuthMessage (Tuple.second updatedAuth)
            )

        NewPersistedToken token ->
            ( model
            , Cmd.map (always AuthMessage (GotPersistedToken token)) Cmd.none
            )

        NoOp ->
            noOp model


noOp : Model -> ( Model, Cmd Msg )
noOp model =
    ( model, Cmd.none )



--
-- View
--


type alias ViewConfiguration msg =
    { title : String
    , btnClass : Html.Attribute msg
    }


view : ViewConfiguration Msg -> Model -> Document Msg
view ({ title } as config) model =
    let
        bootstrap =
            []
    in
    case model.auth.flow of
        Types.Done user ->
            { title = title
            , body = bootstrap ++ [ mapView [] ]
            }

        _ ->
            { title = title
            , body = viewBody config model
            }


mapView : List (Html.Attribute msg) -> Html.Html msg
mapView att =
    Html.node "seznam-maps" [ id "maps", attribute "height" "100vh", attribute "width" "100vw" ] []


viewMap : ViewConfiguration msg -> Model -> List (Html.Html Msg)
viewMap config model =
    [ case model.auth.flow of
        Types.Authorized _ ->
            mapView
                [ style "height" "380px" ]

        _ ->
            mapView
                [ style "height" "380px" ]
    ]


viewBody : ViewConfiguration Msg -> Model -> List (Html.Html Msg)
viewBody config model =
    let
        authorized =
            Html.div [ class "flex" ]
                [ viewAuthorizationStep True
                , viewStepSeparator True
                , viewGetUserInfoStep False
                ]
                :: viewAuthorized
    in
    [ Html.div [ class "flex", class "flex-column", class "flex-space-around" ] <|
        case model.auth.flow of
            Types.Idle ->
                Html.div [ class "flex" ]
                    [ viewAuthorizationStep False
                    , viewStepSeparator False
                    , viewGetUserInfoStep False
                    ]
                    :: viewIdle config

            Types.Authorized _ ->
                authorized

            Types.Done userInfo ->
                Html.div [ class "flex" ]
                    [ viewAuthorizationStep True
                    , viewStepSeparator True
                    , viewGetUserInfoStep True
                    ]
                    :: viewUserInfo config userInfo

            Types.Errored err ->
                Html.div [ class "flex" ]
                    [ viewErroredStep
                    ]
                    :: List.map (\x -> Html.map AuthMessage x) (viewErrored err)
    ]


viewIdle : ViewConfiguration Msg -> List (Html.Html Msg)
viewIdle { btnClass } =
    List.map (Html.map AuthMessage) (Auth.form [])


viewAuthorized : List (Html.Html Msg)
viewAuthorized =
    [ Html.span [] [ Html.text "Getting user info..." ]
    ]


viewUserInfo : ViewConfiguration Msg -> Types.UserInfo -> List (Html.Html Msg)
viewUserInfo { btnClass } { name, picture, email } =
    [ Html.div [ class "flex", class "flex-column" ]
        [ Html.img [ class "avatar", Attributes.src picture ] []
        , Html.p [] [ Html.text name ]
        , Html.p [] [ Html.text email ]
        , Html.div []
            [ Html.button
                [ Events.onClick (AuthMessage Auth.SignOutRequested), btnClass ]
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
        [ Html.span [ style "left" "-50%" ] [ Html.h1 [] [Html.text "Authorization error"] ] ]


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
