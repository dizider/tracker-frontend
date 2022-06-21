port module Ports exposing (..)

import Types exposing (Coordinates)

port randomBytes : (List Int -> msg) -> Sub msg


port genRandomBytes : Int -> Cmd msg


port persistToken : String -> Cmd msg


port getPersistedToken : Bool -> Cmd msg


port persistedToken : (String -> msg) -> Sub msg


port removeToken : () -> Cmd msg


port addTrack : ( Int, String ) -> Cmd msg


port removeTrack : Int -> Cmd msg


port newCoordinatesReceived : (String -> msg) -> Sub msg


port updateCoordinates : List Coordinates -> Cmd msg

port updateTracks : List (String, List Coordinates) -> Cmd msg 

port fullscreenMap : () -> Cmd msg

port subscribeCoordinates : Int -> Cmd msg

port unsubscribeCoordinates : Int -> Cmd msg

port fullscreenActive : (Bool -> msg) -> Sub msg

port loadMap : () -> Cmd msg

port centerMap : Coordinates -> Cmd msg