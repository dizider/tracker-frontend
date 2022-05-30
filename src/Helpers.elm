module Helpers exposing (..)

import Url exposing (Protocol(..), Url)
import Bytes as Bytes
import Bytes.Encode as BEncode
import Base64.Encode as Base64
import OAuth exposing (ErrorCode(..))


toBytes : List Int -> Bytes.Bytes
toBytes =
    List.map BEncode.unsignedInt8 >> BEncode.sequence >> BEncode.encode


base64 : Bytes.Bytes -> String
base64 =
    Base64.bytes >> Base64.encode


convertBytes : List Int -> String
convertBytes =
    toBytes >> base64


oauthErrorToString : { error : OAuth.ErrorCode, errorDescription : Maybe String } -> String
oauthErrorToString { error, errorDescription } =
    let
        desc =
            errorDescription |> Maybe.withDefault "" |> String.replace "+" " "
    in
    OAuth.errorCodeToString error ++ ": " ++ desc


defaultHttpsUrl : Url
defaultHttpsUrl =
    { protocol = Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }
