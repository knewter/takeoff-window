module Simulate exposing (Report, encode, run)

import Ai
import Data
import Engine exposing (Agent(..))
import Json.Encode as Encode


type alias Report =
    { games : Int
    , errors : Int
    , avgRounds : Float
    , median : Int
    , p25 : Int
    , p75 : Int
    , histogram : List ( Int, Int )
    , wins : List ( String, Int )
    , timeouts : Int
    , publicWins : Int
    , secretWins : Int
    , avgDepth : Float
    , pctMultiLevel : Float
    , humanGames : Int
    , humanWins : Int
    , humanWinRate : Float
    , humanByFaction : List ( String, Int, Int )
    }


run : { games : Int, humanGames : Int, seed : Int } -> Report
run opts =
    let
        ai =
            playMany opts.games opts.seed False

        human =
            playMany opts.humanGames (opts.seed + 9000) True

        rounds =
            List.map .rounds ai

        depths =
            List.map .depth ai
    in
    { games = List.length ai
    , errors = List.length (List.filter .error ai) + List.length (List.filter .error human)
    , avgRounds = average (List.map toFloat rounds)
    , median = percentile 50 rounds
    , p25 = percentile 25 rounds
    , p75 = percentile 75 rounds
    , histogram = List.map (\r -> ( r, List.length (List.filter (\n -> n == r) rounds) )) (List.range 1 20)
    , wins = countWins ai
    , timeouts = List.length (List.filter (\g -> g.ending == Timeout) ai)
    , publicWins = List.length (List.filter (\g -> g.ending == Public) ai)
    , secretWins = List.length (List.filter (\g -> g.ending == Secret) ai)
    , avgDepth = average depths
    , pctMultiLevel =
        if List.isEmpty ai then
            0

        else
            toFloat (List.length (List.filter (\g -> g.depth >= 2) ai)) / toFloat (List.length ai)
    , humanGames = List.length human
    , humanWins = List.length (List.filter (\g -> g.winnerSeat == Just 0) human)
    , humanWinRate =
        if List.isEmpty human then
            0

        else
            toFloat (List.length (List.filter (\g -> g.winnerSeat == Just 0) human)) / toFloat (List.length human)
    , humanByFaction = humanLines human
    }


encode : Report -> Encode.Value
encode report =
    Encode.object
        [ ( "games", Encode.int report.games )
        , ( "errors", Encode.int report.errors )
        , ( "avgRounds", Encode.float report.avgRounds )
        , ( "median", Encode.int report.median )
        , ( "p25", Encode.int report.p25 )
        , ( "p75", Encode.int report.p75 )
        , ( "histogram", Encode.list (\( r, n ) -> Encode.object [ ( "round", Encode.int r ), ( "games", Encode.int n ) ]) report.histogram )
        , ( "wins", Encode.object (List.map (\( id, n ) -> ( id, Encode.int n )) report.wins) )
        , ( "winRates", Encode.object (List.map (\( id, n ) -> ( id, Encode.float (rate n report.games) )) report.wins) )
        , ( "timeouts", Encode.int report.timeouts )
        , ( "publicWins", Encode.int report.publicWins )
        , ( "secretWins", Encode.int report.secretWins )
        , ( "avgDepth", Encode.float report.avgDepth )
        , ( "pctMultiLevel", Encode.float report.pctMultiLevel )
        , ( "humanGames", Encode.int report.humanGames )
        , ( "humanWins", Encode.int report.humanWins )
        , ( "humanWinRate", Encode.float report.humanWinRate )
        , ( "humanByFaction"
          , Encode.list
                (\( faction, games, wins ) ->
                    Encode.object
                        [ ( "faction", Encode.string faction )
                        , ( "games", Encode.int games )
                        , ( "wins", Encode.int wins )
                        , ( "winRate", Encode.float (rate wins games) )
                        ]
                )
                report.humanByFaction
          )
        ]


type Ending
    = Timeout
    | Public
    | Secret
    | None


type alias Row =
    { error : Bool
    , rounds : Int
    , depth : Float
    , winner : Maybe String
    , winnerSeat : Maybe Int
    , seatFaction : String
    , ending : Ending
    }


playMany : Int -> Int -> Bool -> List Row
playMany n seed reasonable =
    List.range 0 (n - 1)
        |> List.map
            (\i ->
                let
                    faction =
                        factionAt i

                    game0 =
                        Engine.createGame
                            { seed = seed + i * 97
                            , playerFaction = Just faction
                            , numAI = 3
                            , playerName = "You"
                            }

                    game1 =
                        { game0
                            | players =
                                List.map
                                    (\p ->
                                        if p.id == 0 && reasonable then
                                            { p | agent = Reasonable, isAI = False }

                                        else
                                            { p | agent = Rival, isAI = True }
                                    )
                                    game0.players
                        }

                    done =
                        Ai.run 900 game1

                    reason =
                        Maybe.withDefault "" done.winReason
                in
                { error = not done.gameOver
                , rounds = min 20 done.round
                , depth = treeDepth done
                , winner = Maybe.map (\id -> (Engine.getPlayer done id).factionId) done.winner
                , winnerSeat = done.winner
                , seatFaction = (Engine.getPlayer game1 0).factionId
                , ending =
                    if not done.gameOver then
                        None

                    else if String.contains "window closed" reason then
                        Timeout

                    else if String.contains "public influence" reason then
                        Public

                    else
                        Secret
                }
            )


factionAt : Int -> String
factionAt i =
    let
        ids =
            List.map .id Data.factions
    in
    List.drop (modBy (List.length ids) i) ids |> List.head |> Maybe.withDefault "helios"


humanLines : List Row -> List ( String, Int, Int )
humanLines rows =
    List.map
        (\id ->
            let
                played =
                    List.filter (\row -> row.seatFaction == id) rows

                wins =
                    List.filter (\row -> row.winnerSeat == Just 0) played
            in
            ( id, List.length played, List.length wins )
        )
        (List.map .id Data.factions)


countWins : List Row -> List ( String, Int )
countWins rows =
    List.map
        (\id ->
            ( id
            , List.length (List.filter (\row -> row.winner == Just id) rows)
            )
        )
        (List.map .id Data.factions)


treeDepth : Engine.Game -> Float
treeDepth game =
    game.players
        |> List.map (\p -> toFloat (depthOf p.grove Engine.Patron))
        |> List.foldl max 0


depthOf : List Engine.Group -> Engine.Parent -> Int
depthOf grove parent =
    let
        kids =
            List.filter (\g -> g.parent == Just parent) grove
    in
    List.foldl (\g m -> max m (1 + depthOf grove (Engine.Under g.instanceId))) 0 kids


average : List Float -> Float
average xs =
    case xs of
        [] ->
            0

        _ ->
            List.sum xs / toFloat (List.length xs)


percentile : Int -> List Int -> Int
percentile p xs =
    case List.sort xs of
        [] ->
            0

        sorted ->
            let
                idx =
                    min (List.length sorted - 1) (floor (toFloat (List.length sorted) * toFloat p / 100))
            in
            List.drop idx sorted |> List.head |> Maybe.withDefault 0


rate : Int -> Int -> Float
rate n total =
    if total == 0 then
        0

    else
        toFloat n / toFloat total
