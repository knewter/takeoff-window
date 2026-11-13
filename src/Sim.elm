port module Sim exposing (main)

import Json.Decode as Decode
import Json.Encode as Encode
import Simulate


port emit : String -> Cmd msg


main : Program Decode.Value () msg
main =
    Platform.worker
        { init =
            \flags ->
                let
                    opts =
                        decode flags
                in
                ( (), emit (Encode.encode 0 (Simulate.encode (Simulate.run opts))) )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


decode : Decode.Value -> { games : Int, humanGames : Int, seed : Int }
decode flags =
    Decode.decodeValue
        (Decode.map3 (\g h s -> { games = g, humanGames = h, seed = s })
            (Decode.field "games" Decode.int)
            (Decode.field "humanGames" Decode.int)
            (Decode.field "seed" Decode.int)
        )
        flags
        |> Result.withDefault { games = 360, humanGames = 240, seed = 2026 }
