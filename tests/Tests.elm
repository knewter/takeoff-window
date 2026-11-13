module Tests exposing (suite)

import Engine exposing (Action(..), Attacker(..), Kind(..), Parent(..))
import Expect
import Rng
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Takeoff Window"
        [ describe "rng"
            [ test "mulberry32 matches the JS sequence for seed 2026" <|
                \_ ->
                    floats 8 2026
                        |> Expect.equal
                            [ 0.4554076993
                            , 0.308496146
                            , 0.6611574492
                            , 0.6184752183
                            , 0.1522801018
                            , 0.2218433102
                            , 0.6828599116
                            , 0.801408699
                            ]
            , test "seedFrom accepts decimal seeds and hashes other text" <|
                \_ ->
                    Expect.equal ( 2026, 310164755 ) ( Rng.seedFrom "2026", Rng.seedFrom "takeoff" )
            ]
        , describe "attack resolution"
            [ test "a roll of 12 always fails" <|
                \_ ->
                    let
                        game =
                            fresh "helios"

                        target =
                            List.head game.uncontrolled |> expectGroup
                    in
                    case Engine.applyAction (Engine.withRolls [ ( 6, 6 ) ] game) (Attack Destroy PatronAtk target.instanceId 3 Nothing) of
                        result ->
                            Expect.all
                                [ \_ -> Expect.equal True result.ok
                                , \_ -> Expect.equal (Just False) (Maybe.map .success result.game.lastRoll)
                                , \_ -> Expect.equal True (List.any (\g -> g.instanceId == target.instanceId) result.game.uncontrolled)
                                ]
                                ()
            , test "a low roll succeeds and strength rises with money spent" <|
                \_ ->
                    let
                        game =
                            fresh "helios"

                        target =
                            List.head game.uncontrolled |> expectGroup

                        low =
                            Engine.attackStrength game 0 PatronAtk target.instanceId Destroy 0 0

                        high =
                            Engine.attackStrength game 0 PatronAtk target.instanceId Destroy 2 0
                    in
                    Expect.equal (low.strength + 2) high.strength
            , test "shared alignment helps a control attempt" <|
                \_ ->
                    let
                        game0 =
                            fresh "helios"

                        game1 =
                            Engine.ownCard game0 1 "memory-cartel" Patron

                        game2 =
                            Engine.ownCard game1 0 "green-jacket" Patron

                        jacket =
                            findCard game2 0 "green-jacket"

                        cartel =
                            findCard game2 1 "memory-cartel"

                        b =
                            Engine.attackStrength game2 0 (GroupAtk jacket.instanceId) cartel.instanceId Control 0 0
                    in
                    Expect.all
                        [ \_ -> Expect.equal 3 b.aMod
                        , \_ -> Expect.equal 1 b.special
                        ]
                        ()
            , test "a successful control takes the group and a second attack is refused" <|
                \_ ->
                    let
                        game =
                            fresh "helios"

                        target =
                            List.head game.uncontrolled |> expectGroup

                        first =
                            Engine.applyAction (Engine.withRolls [ ( 1, 1 ) ] game) (Attack Control PatronAtk target.instanceId 3 Nothing)

                        second =
                            Engine.applyAction (Engine.withRolls [ ( 1, 1 ) ] first.game) (Attack Destroy PatronAtk target.instanceId 3 Nothing)
                    in
                    Expect.all
                        [ \_ -> Expect.equal True first.ok
                        , \_ -> Expect.equal (Just True) (Maybe.map .success first.game.lastRoll)
                        , \_ -> Expect.equal (Just 0) (ownedBy first.game target.instanceId)
                        , \_ -> Expect.equal False (List.any (\g -> g.instanceId == target.instanceId) first.game.uncontrolled)
                        , \_ -> Expect.equal False second.ok
                        , \_ -> Expect.equal True (logHas "You control " first.game)
                        , \_ -> Expect.equal False (logHas "You controls" first.game)
                        ]
                        ()
            ]
        , describe "income"
            [ test "opening income uses the plural verb for You" <|
                \_ ->
                    let
                        game =
                            fresh "helios"
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (logHas "You collect " game)
                        , \_ -> Expect.equal False (logHas "You collects" game)
                        ]
                        ()
            ,             test "Long Plan chipmakers and Crown power stacks pay their bonuses" <|
                \_ ->
                    let
                        long =
                            fresh "long-plan"
                                |> quiet
                                |> (\g -> Engine.ownCard g 0 "green-jacket" Patron)
                                |> (\g -> Engine.applyIncome g 0)

                        chip =
                            findCard long 0 "green-jacket"

                        crown =
                            fresh "crown"
                                |> quiet
                                |> (\g -> Engine.ownCard g 0 "interconnect" Patron)
                                |> (\g -> Engine.ownCard g 0 "regional-grid" Patron)
                                |> (\g -> Engine.applyIncome g 0)

                        grid =
                            findCard crown 0 "regional-grid"
                    in
                    Expect.all
                        [ \_ -> Expect.equal 5 chip.money
                        , \_ -> Expect.equal 3 (Engine.getPlayer long 0).compute
                        , \_ -> Expect.equal 4 grid.money
                        ]
                        ()
            ]
        , describe "control, neutralize, destroy"
            [ test "neutralize returns a group and its subordinate to the open field" <|
                \_ ->
                    let
                        ( game, parentId, childId ) =
                            nestOwned 1

                        result =
                            Engine.applyAction (Engine.withRolls [ ( 1, 1 ) ] game) (Attack Neutralize PatronAtk parentId 3 Nothing)
                    in
                    Expect.all
                        [ \_ -> Expect.equal True result.ok
                        , \_ -> Expect.equal Nothing (ownedBy result.game parentId)
                        , \_ -> Expect.equal Nothing (ownedBy result.game childId)
                        , \_ -> Expect.equal True (List.any (\g -> g.instanceId == parentId && g.parent == Nothing) result.game.uncontrolled)
                        , \_ -> Expect.equal True (List.any (\g -> g.instanceId == childId && g.parent == Nothing) result.game.uncontrolled)
                        ]
                        ()
            , test "destroy removes the target and frees subordinates" <|
                \_ ->
                    let
                        ( game, parentId, childId ) =
                            nestOwned 1

                        result =
                            Engine.applyAction (Engine.withRolls [ ( 1, 1 ) ] game) (Attack Destroy PatronAtk parentId 3 Nothing)
                    in
                    Expect.all
                        [ \_ -> Expect.equal True result.ok
                        , \_ -> Expect.equal Nothing (ownedBy result.game parentId)
                        , \_ -> Expect.equal False (List.any (\g -> g.instanceId == parentId) result.game.uncontrolled)
                        , \_ -> Expect.equal True (List.any (\g -> g.instanceId == childId && g.parent == Nothing) result.game.uncontrolled)
                        ]
                        ()
            , test "destroying an uncontrolled group removes it from the window" <|
                \_ ->
                    let
                        game =
                            fresh "pause"

                        target =
                            List.head game.uncontrolled |> expectGroup

                        result =
                            Engine.applyAction (Engine.withRolls [ ( 1, 1 ) ] game) (Attack Destroy PatronAtk target.instanceId 3 Nothing)
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Just True) (Maybe.map .success result.game.lastRoll)
                        , \_ -> Expect.equal False (stillInPlay result.game target.instanceId)
                        ]
                        ()
            ]
        , describe "win conditions"
            [ test "public influence requires six settled groups" <|
                \_ ->
                    let
                        won =
                            ownMany "crown" [ "regional-grid", "desert-builders", "green-jacket", "rivercloud", "model-bureau", "podcast-circuit" ]

                        privileged =
                            { won
                                | players =
                                    List.map
                                        (\p ->
                                            { p | grove = List.map (\g -> { g | privilegedTurns = 2 }) p.grove }
                                        )
                                        won.players
                            }
                    in
                    Expect.all
                        [ \_ -> expectContains "public influence" (Engine.victory won 0)
                        , \_ -> Expect.equal Nothing (Engine.victory privileged 0)
                        ]
                        ()
            , test "Helios wins at 30 compute" <|
                \_ ->
                    expectContains "takeoff" (Engine.victory (setCompute (fresh "helios") 0 30) 0)
            , test "Open Wave wins from a leak plus three Open groups" <|
                \_ ->
                    let
                        game0 =
                            ownMany "open-wave" [ "open-weights", "the-timeline", "accel-institute" ]

                        flags =
                            game0.flags
                    in
                    expectContains "weights are out" (Engine.victory { game0 | flags = { flags | weightsLeak = True } } 0)
            , test "Open Wave also wins with the collective and four groups, even while privileged" <|
                \_ ->
                    let
                        game =
                            ownMany "open-wave" [ "open-weights", "podcast-circuit", "soft-landing", "care-forum" ]

                        privileged =
                            { game
                                | players =
                                    List.map
                                        (\p -> { p | grove = List.map (\g -> { g | privilegedTurns = 2 }) p.grove })
                                        game.players
                            }
                    in
                    expectContains "Open Weights Collective" (Engine.victory privileged 0)
            , test "Pause wins by removing two Frontier groups" <|
                \_ ->
                    let
                        game =
                            fresh "pause"
                                |> (\g ->
                                        let
                                            p =
                                                Engine.getPlayer g 0
                                        in
                                        { g
                                            | players =
                                                List.map
                                                    (\q ->
                                                        if q.id == 0 then
                                                            { p | capturedFrontier = [ "first-letter", "the-garden" ] }

                                                        else
                                                            q
                                                    )
                                                    g.players
                                        }
                                   )
                    in
                    expectContains "two Frontier" (Engine.victory game 0)
            , test "Pause also wins with a regulator, a headline, and a third group" <|
                \_ ->
                    expectContains "referee" (Engine.victory (ownMany "pause" [ "model-bureau", "prestige-mag", "care-forum" ]) 0)
            , test "Directorate wins with a frontier lab, a regulator, and five settled groups" <|
                \_ ->
                    expectContains "Frontier lab" (Engine.victory (ownMany "directorate" [ "first-letter", "model-bureau", "podcast-circuit", "soft-landing", "desert-builders" ]) 0)
            , test "Directorate also wins with five groups and two Nationalist ones" <|
                \_ ->
                    expectContains "nationalist" (Engine.victory (ownMany "directorate" [ "island-foundry", "export-desk", "podcast-circuit", "soft-landing", "desert-builders" ]) 0)
            , test "Long Plan wins with a fab, a cloud, and five settled groups" <|
                \_ ->
                    expectContains "fab, the cloud" (Engine.victory (ownMany "long-plan" [ "island-foundry", "rivercloud", "podcast-circuit", "soft-landing", "desert-builders" ]) 0)
            , test "Long Plan also wins with a compute lead" <|
                \_ ->
                    let
                        game =
                            setCompute (fresh "long-plan") 0 24
                    in
                    expectContains "sovereign compute" (Engine.victory game 0)
            , test "Crown wins by owning power, concrete, and silicon" <|
                \_ ->
                    expectContains "power, concrete" (Engine.victory (ownMany "crown" [ "regional-grid", "desert-builders", "green-jacket" ]) 0)
            , test "Crown also wins as the liquid sovereign" <|
                \_ ->
                    let
                        game =
                            ownMany "crown" [ "podcast-circuit", "soft-landing", "care-forum", "accel-institute" ]

                        rich =
                            { game
                                | players =
                                    List.map
                                        (\p ->
                                            if p.id == 0 then
                                                { p | money = Engine.crownMoney }

                                            else
                                                p
                                        )
                                        game.players
                            }
                    in
                    expectContains "liquid sovereign" (Engine.victory rich 0)
            ]
        ]


fresh : String -> Engine.Game
fresh faction =
    Engine.createGame { seed = 3, playerFaction = Just faction, numAI = 2, playerName = "You" }


quiet : Engine.Game -> Engine.Game
quiet game =
    { game
        | flags = { weightsLeak = False, gpuShortage = 0, exportControls = 0, visaFreeze = 0, safetyLetter = 0 }
        , effects = []
        , players = List.map (\p -> { p | compute = 0 }) game.players
    }


ownMany : String -> List String -> Engine.Game
ownMany faction ids =
    List.foldl (\id g -> Engine.ownCard g 0 id Patron) (fresh faction) ids


setCompute : Engine.Game -> Int -> Int -> Engine.Game
setCompute game pid n =
    { game | players = List.map (\p -> if p.id == pid then { p | compute = n } else p) game.players }


nestOwned : Int -> ( Engine.Game, String, String )
nestOwned ownerId =
    let
        game1 =
            Engine.ownCard (fresh "helios") ownerId "podcast-circuit" Patron

        parent =
            findCard game1 ownerId "podcast-circuit"

        game2 =
            Engine.ownCard game1 ownerId "accel-institute" (Under parent.instanceId)

        child =
            findCard game2 ownerId "accel-institute"
    in
    ( game2, parent.instanceId, child.instanceId )


findCard : Engine.Game -> Int -> String -> Engine.Group
findCard game pid cardId =
    Engine.getPlayer game pid
        |> .grove
        |> List.filter (\g -> g.cardId == cardId)
        |> List.head
        |> expectGroup


ownedBy : Engine.Game -> String -> Maybe Int
ownedBy game iid =
    Engine.findInstance game iid |> Maybe.andThen .ownerId


stillInPlay : Engine.Game -> String -> Bool
stillInPlay game iid =
    Engine.findInstance game iid /= Nothing


logHas : String -> Engine.Game -> Bool
logHas needle game =
    List.any (\entry -> String.contains needle entry.text) game.log


expectContains : String -> Maybe String -> Expect.Expectation
expectContains needle result =
    case result of
        Just text ->
            if String.contains needle text then
                Expect.pass

            else
                Expect.fail ("Expected <<" ++ needle ++ ">> in <<" ++ text ++ ">>")

        Nothing ->
            Expect.fail ("Expected a win containing " ++ needle)


expectGroup : Maybe Engine.Group -> Engine.Group
expectGroup g =
    case g of
        Just group ->
            group

        Nothing ->
            { instanceId = "missing"
            , cardId = "missing"
            , money = -999
            , ownerId = Nothing
            , parent = Nothing
            , exhausted = False
            , privilegedTurns = 0
            , powerMod = 0
            , powerModTurns = 0
            , extraAlignments = []
            , disabledTurns = 0
            }


floats : Int -> Int -> List Float
floats n rng =
    if n <= 0 then
        []

    else
        let
            ( u, next ) =
                Rng.nextFloat rng
        in
        round10 u :: floats (n - 1) next


round10 : Float -> Float
round10 n =
    toFloat (round (n * 10000000000)) / 10000000000
