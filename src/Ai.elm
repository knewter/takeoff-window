module Ai exposing (chooseAI, chooseHuman, run)

import Data exposing (Card)
import Engine exposing (Action(..), Attacker(..), Kind(..), Parent(..))
import Rng


{-| Play until the window closes or the guard runs out. `reasonable` makes
player agents tagged Reasonable use the scripted human policy.
-}
run : Int -> Engine.Game -> Engine.Game
run guard game =
    if game.gameOver || guard <= 0 then
        game

    else
        let
            ( action, game1 ) =
                decide game

            result =
                Engine.applyAction game1 action

            next =
                if result.ok then
                    result.game

                else
                    (Engine.applyAction result.game Pass).game
        in
        run (guard - 1) next


decide : Engine.Game -> ( Action, Engine.Game )
decide game =
    let
        player =
            Engine.current game
    in
    if player.agent == Engine.Reasonable then
        ( chooseHuman game, game )

    else
        chooseAI game


chooseAI : Engine.Game -> ( Action, Engine.Game )
chooseAI game =
    let
        player =
            Engine.current game

        actions =
            Engine.legalActions game

        scored =
            List.map (\a -> ( scoreAction game player a, a )) actions
                |> sortDesc

        top =
            case scored of
                ( s, _ ) :: _ ->
                    s

                [] ->
                    -999
    in
    case scored of
        [] ->
            ( Pass, game )

        ( bestS, bestA ) :: _ ->
            if bestS < 0.2 && isWeakAttack game bestA then
                ( Pass, game )

            else
                let
                    pool =
                        List.filter (\( s, _ ) -> s >= top - 1.5 && s > -5) scored
                in
                if List.isEmpty pool then
                    ( Pass, game )

                else
                    let
                        ( u, rng1 ) =
                            Rng.nextFloat game.rng

                        idx =
                            floor (u * toFloat (List.length pool))

                        picked =
                            List.drop idx pool |> List.head |> Maybe.map Tuple.second |> Maybe.withDefault Pass
                    in
                    ( picked, { game | rng = rng1 } )


chooseHuman : Engine.Game -> Action
chooseHuman game =
    let
        player =
            Engine.current game

        scored =
            Engine.legalActions game
                |> List.map (\a -> ( scoreHuman game player a, a ))
                |> sortDesc
    in
    case scored of
        ( s, action ) :: _ ->
            if s < 0.6 then
                Pass

            else
                action

        [] ->
            Pass


isWeakAttack : Engine.Game -> Action -> Bool
isWeakAttack game action =
    case action of
        Attack kind attacker targetId spend _ ->
            (Engine.attackStrength game (Engine.current game).id attacker targetId kind spend 0).prob < 0.12

        _ ->
            False


scoreAction : Engine.Game -> Engine.Player -> Action -> Float
scoreAction game player action =
    case action of
        Pass ->
            -1

        Drop id ->
            if Engine.groupCount player < 3 then
                -50

            else
                case Engine.findInstance game id of
                    Just g ->
                        -(desire player g)

                    Nothing ->
                        -50

        Reorg _ _ ->
            -8

        Leak ->
            14 + (Engine.goalProgress game player).secretRatio * 8

        Attack kind _ targetId spend _ ->
            case Engine.findInstance game targetId of
                Nothing ->
                    -100

                Just target ->
                    let
                        b =
                            attackBreakdown game player action

                        prob =
                            b.prob

                        value =
                            case kind of
                                Control ->
                                    6 + desire player target + (if target.ownerId /= Nothing then threat game player target * 0.6 + 2 else 0)

                                Neutralize ->
                                    2
                                        + threat game player target
                                        + (if player.factionId == "pause" && List.member "Frontier" (Engine.cardOf target).traits then
                                            10

                                           else
                                            0
                                          )

                                Destroy ->
                                    1
                                        + threat game player target * 0.8
                                        + (if player.factionId == "pause" && List.member "Frontier" (Engine.cardOf target).traits then
                                            12

                                           else
                                            0
                                          )
                                        + (if player.factionId /= "pause" then
                                            -(desire player target) * 0.5

                                           else
                                            0
                                          )

                        ev =
                            value * prob - toFloat spend * 0.35
                    in
                    if prob < 0.12 && Engine.groupCount player < 3 then
                        -20

                    else if toFloat spend > toFloat (Engine.totalMoney player) * 0.6 then
                        ev - 2

                    else
                        ev + alignBonus b


scoreHuman : Engine.Game -> Engine.Player -> Action -> Float
scoreHuman game player action =
    case action of
        Pass ->
            -0.4

        Drop id ->
            if Engine.groupCount player < 4 then
                -40

            else
                case Engine.findInstance game id of
                    Just g ->
                        if desire player g < 3 then
                            1

                        else
                            -20

                    Nothing ->
                        -20

        Reorg id parent ->
            case Engine.findInstance game id of
                Just g ->
                    if (Engine.cardOf g).sockets <= 1 && parent /= Patron && Engine.socketsUsed player Patron >= 2 then
                        1.2

                    else
                        -6

                Nothing ->
                    -6

        Leak ->
            18

        Attack kind attacker targetId spend _ ->
            case Engine.findInstance game targetId of
                Nothing ->
                    -100

                Just target ->
                    let
                        b =
                            Engine.attackStrength game player.id attacker targetId kind spend 0

                        bonus =
                            goalBonus player.factionId (Engine.cardOf target) kind

                        value =
                            case kind of
                                Control ->
                                    7 + desire player target * 1.15 + bonus + visibleThreat game player target

                                Neutralize ->
                                    1 + visibleThreat game player target + (if player.factionId == "pause" then bonus else 0) - desire player target * 0.35

                                Destroy ->
                                    visibleThreat game player target * 0.7 + (if player.factionId == "pause" then bonus else -(desire player target) * 0.8)

                        ev =
                            value * b.prob - toFloat spend * 0.45
                    in
                    if b.prob < 0.18 && Engine.groupCount player < 2 then
                        -8

                    else
                        ev


attackBreakdown : Engine.Game -> Engine.Player -> Action -> Engine.Breakdown
attackBreakdown game player action =
    case action of
        Attack kind attacker targetId spend _ ->
            Engine.attackStrength game player.id attacker targetId kind spend 0

        _ ->
            Engine.attackStrength game player.id PatronAtk "" Control 0 0


alignBonus : Engine.Breakdown -> Float
alignBonus b =
    if b.aMod > 0 then
        1.5

    else
        0


desire : Engine.Player -> Engine.Group -> Float
desire player g =
    let
        card =
            Engine.cardOf g

        w =
            wanted player.factionId
    in
    (if List.member card.id w.prefer then
        8

     else
        0
    )
        + (if List.member card.type_ w.types then
            5

           else
            0
          )
        + (if List.any (\a -> List.member a w.aligns) card.alignments then
            3

           else
            0
          )
        + (if List.any (\t -> List.member t w.traits) card.traits then
            3

           else
            0
          )
        + toFloat card.income * 0.8
        + toFloat card.compute * 2
        + toFloat card.power * 0.3
        - toFloat (max 0 (card.resistance - 5)) * 0.4


goalBonus : String -> Card -> Kind -> Float
goalBonus faction card kind =
    case faction of
        "helios" ->
            toFloat card.compute * 3

        "open-wave" ->
            (if List.member "Open" card.alignments || card.id == "open-weights" then
                9

             else
                0
            )
                + (if card.type_ == "lab" then
                    4

                   else
                    0
                  )

        "pause" ->
            (if kind /= Control && List.member "Frontier" card.traits then
                12

             else
                0
            )
                + (if card.type_ == "regulator" || card.type_ == "media" then
                    7

                   else
                    0
                  )

        "directorate" ->
            (if List.member "Frontier" card.traits || card.type_ == "regulator" then
                7

             else
                0
            )
                + (if List.member "Nationalist" card.alignments then
                    5

                   else
                    0
                  )

        "long-plan" ->
            (if card.type_ == "chipmaker" || card.type_ == "hyperscaler" then
                8

             else
                0
            )
                + toFloat card.compute * 2

        "crown" ->
            if card.type_ == "power" || card.type_ == "datacenter" || card.type_ == "chipmaker" then
                8

            else
                toFloat card.income

        _ ->
            0


visibleThreat : Engine.Game -> Engine.Player -> Engine.Group -> Float
visibleThreat game player target =
    case target.ownerId of
        Nothing ->
            0

        Just oid ->
            if oid == player.id then
                0

            else
                let
                    owner =
                        Engine.getPlayer game oid

                    groups =
                        Engine.groupCount owner
                in
                (if groups >= 4 then
                    5

                 else if groups >= 3 then
                    2

                 else
                    0
                )
                    + toFloat groups * 0.4
                    + toFloat owner.compute * 0.08


threat : Engine.Game -> Engine.Player -> Engine.Group -> Float
threat game player target =
    case target.ownerId of
        Just oid ->
            if oid == player.id then
                0

            else
                let
                    owner =
                        Engine.getPlayer game oid

                    before =
                        (Engine.goalProgress game owner).ratio
                in
                (if before >= 0.55 then
                    4 + before * 6

                 else
                    0
                )
                    + desire owner target * 0.5
                    + (if Engine.groupCount owner >= 3 then
                        3

                       else
                        0
                      )

        Nothing ->
            0


type alias Want =
    { types : List String
    , aligns : List String
    , traits : List String
    , prefer : List String
    }


wanted : String -> Want
wanted faction =
    case faction of
        "helios" ->
            Want [ "chipmaker", "hyperscaler", "lab", "datacenter" ] [ "Accelerationist", "Corporate" ] [ "Compute", "Frontier" ] []

        "open-wave" ->
            Want [ "lab", "media", "thinktank" ] [ "Open", "Accelerationist" ] [ "OpenSource" ] [ "open-weights" ]

        "pause" ->
            Want [ "regulator", "media", "thinktank", "lab" ] [ "Safety", "Academic" ] [ "Frontier", "Regulator" ] []

        "directorate" ->
            Want [ "lab", "regulator", "talent" ] [ "Nationalist" ] [ "Frontier", "Founder", "Regulator" ] []

        "long-plan" ->
            Want [ "chipmaker", "hyperscaler", "datacenter" ] [ "Nationalist", "Corporate" ] [ "Compute" ] []

        "crown" ->
            Want [ "power", "datacenter", "chipmaker", "vc" ] [ "Corporate", "Nationalist" ] [ "Power", "Datacenter" ] []

        _ ->
            Want [] [] [] []


sortDesc : List ( Float, a ) -> List ( Float, a )
sortDesc items =
    List.sortBy (\( s, _ ) -> negate (round (s * 1000))) items
