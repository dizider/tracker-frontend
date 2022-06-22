module DecodersTest exposing (..)

import Decoders
import Expect
import Json.Decode
import Test exposing (..)
import Types


tracker1 : Types.Tracker
tracker1 =
    { id = 1
    , name = "tracker1"
    , track =
        { id = 2
        , trackerId = 1
        , name = "track1"
        , visitedWaypoints = 0
        }
    }


tracker2 : Types.Tracker
tracker2 =
    { id = 2
    , name = "tracker2"
    , track =
        { id = 3
        , trackerId = 1
        , name = "track3"
        , visitedWaypoints = 0
        }
    }


track1 : Types.Track
track1 =
    { id = 2
    , trackerId = 1
    , name = "track1"
    , visitedWaypoints = 2
    }


track2 : Types.Track
track2 =
    { id = 3
    , trackerId = 2
    , name = "track2"
    , visitedWaypoints = 0
    }


coordinates1 : Types.Coordinates
coordinates1 =
    { id = 1
    , trackId = 2
    , lat = 12.12
    , lon = 11.11
    , time = "1655903945"
    , alt = 13.13
    , battery = 3425.05
    }


coordinates2 : Types.Coordinates
coordinates2 =
    { id = 2
    , trackId = 2
    , lat = 12.12
    , lon = 11.11
    , time = "1655903945"
    , alt = 13.13
    , battery = 3425.05
    }


all : Test
all =
    describe "Decoders"
        [ describe "Track"
            [ test "Decode valid track 1" <|
                \_ ->
                    "{\"id\":2,\"trackerId\":1,\"name\":\"track1\",\"visitedWaypoints\":2}"
                        |> Json.Decode.decodeString Decoders.decodeTrack
                        |> Expect.equal (Ok track1)
            , test "Decode valid track 2" <|
                \_ ->
                    "{\"id\":3,\"trackerId\":2,\"name\":\"track2\",\"visitedWaypoints\":0}"
                        |> Json.Decode.decodeString Decoders.decodeTrack
                        |> Expect.equal (Ok track2)
            , test "Decode list of tracks" <|
                \_ ->
                    "[{\"id\":3,\"trackerId\":2,\"name\":\"track2\",\"visitedWaypoints\":0},{\"id\":2,\"trackerId\":1,\"name\":\"track1\",\"visitedWaypoints\":2}]"
                        |> Json.Decode.decodeString Decoders.decodeTrackList
                        |> (\x ->
                                case x of
                                    Ok list ->
                                        Expect.equalLists list [ track2, track1 ]

                                    Err _ ->
                                        Expect.fail "Decoding error"
                           )
            , test "Decode invalid list of tracks" <|
                \_ ->
                    "{\"id\":3,\"trackerId\":2,\"name\":\"track2\",\"visitedWaypoints\":0},{\"id\":2,\"trackerId\":1,\"name\":\"track1\",\"visitedWaypoints\":2}"
                        |> Json.Decode.decodeString Decoders.decodeTrackList
                        |> (\x ->
                                case x of
                                    Ok _ ->
                                        Expect.fail "Invalid list of tracks was successfully decoded"

                                    Err _ ->
                                        Expect.pass
                           )
            , test "Space insensivity" <|
                \_ ->
                    "{\"id\":2,\"trackerId\"     :1,\n \"name\":\t\t\t\"track1\",\"visitedWaypoints\":2}"
                        |> Json.Decode.decodeString Decoders.decodeTrack
                        |> Expect.equal (Ok track1)
            , test "Decode invalid id" <|
                \_ ->
                    "{\"id\":a,\"trackerId\":1,\"name\":\"track1\",\"visitedWaypoints\":2}"
                        |> Json.Decode.decodeString Decoders.decodeTrack
                        |> Expect.err
            , test "Decode invalid visited waypoints count" <|
                \_ ->
                    "{\"id\":21,\"trackerId\":1,\"name\":\"track1\",\"visitedWaypoints\":f}"
                        |> Json.Decode.decodeString Decoders.decodeTrack
                        |> Expect.err
            , test "Decode invalid tracker id" <|
                \_ ->
                    "{\"id\":21,\"trackerId\":a,\"name\":\"track1\",\"visitedWaypoints\":25}"
                        |> Json.Decode.decodeString Decoders.decodeTrack
                        |> Expect.err
            ]
        , describe "Tracker"
            [ test "Decode valid tracker 1" <|
                \_ ->
                    "{\"id\":1,\"name\":\"tracker1\",\"track\":{\"id\":2,\"trackerId\":1,\"name\":\"track1\",\"visitedWaypoints\":0}}"
                        |> Json.Decode.decodeString Decoders.decodeTracker
                        |> Expect.equal (Ok tracker1)
            , test "Decode valid tracker 2" <|
                \_ ->
                    "{\"id\":2,\"name\":\"tracker2\",\"track\":{\"id\":3,\"trackerId\":1,\"name\":\"track3\",\"visitedWaypoints\":0}}"
                        |> Json.Decode.decodeString Decoders.decodeTracker
                        |> Expect.equal (Ok tracker2)
            , test "Decode list of trackers" <|
                \_ ->
                    "[{\"id\":1,\"name\":\"tracker1\",\"track\":{\"id\":2,\"trackerId\":1,\"name\":\"track1\",\"visitedWaypoints\":0}}, {\"id\":2,\"name\":\"tracker2\",\"track\":{\"id\":3,\"trackerId\":1,\"name\":\"track3\",\"visitedWaypoints\":0}}]"
                        |> Json.Decode.decodeString Decoders.decodeTrackerList
                        |> (\x ->
                                case x of
                                    Ok list ->
                                        Expect.equalLists list [ tracker1, tracker2 ]

                                    Err _ ->
                                        Expect.fail "Decoding error"
                           )
            , test "Decode invalid list of trackers" <|
                \_ ->
                    "{{\"id\":1,\"name\":\"tracker1\",\"track\":{\"id\":2,\"trackerId\":1,\"name\":\"track1\",\"visitedWaypoints\":0}}, {\"id\":2,\"name\":\"tracker2\",\"track\":{\"id\":3,\"trackerId\":1,\"name\":\"track3\",\"visitedWaypoints\":0}}}"
                        |> Json.Decode.decodeString Decoders.decodeTrackerList
                        |> (\x ->
                                case x of
                                    Ok _ ->
                                        Expect.fail "Invalid list of trackers was successfully decoded"

                                    Err _ ->
                                        Expect.pass
                           )
            , test "Decode invalid id" <|
                \_ ->
                    "{\"id\":as,\"name\":\"tracker1\",\"track\":{\"id\":2,\"trackerId\":1,\"name\":\"track1\",\"visitedWaypoints\":0}}"
                        |> Json.Decode.decodeString Decoders.decodeTracker
                        |> Expect.err
            ]
        , describe "Coordinates"
            [ test "Decode valid coordinates" <|
                \_ ->
                    "{\"alt\":13.13,\"battery\":3425.05,\"id\":1,\"lat\":12.12,\"lon\":11.11,\"time\":\"1655903945\",\"trackId\":2}"
                        |> Json.Decode.decodeString Decoders.decodeCoordinates
                        |> Expect.equal (Ok coordinates1)
            ]
        ]
