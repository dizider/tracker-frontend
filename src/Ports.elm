port module Ports exposing (..)


port randomBytes : (List Int -> msg) -> Sub msg


port genRandomBytes : Int -> Cmd msg


port persistToken : String -> Cmd msg


port getPersistedToken : Bool -> Cmd msg


port persistedToken : (String -> msg) -> Sub msg

port removeToken : Bool -> Cmd msg

port addTrack : (Int, String) -> Cmd msg

port removeTrack : Int -> Cmd msg