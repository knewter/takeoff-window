module Engine exposing
    ( ActResult
    , Action(..)
    , Agent(..)
    , Attacker(..)
    , Breakdown
    , Dice
    , EventView
    , Flags
    , Game
    , Group
    , Kind(..)
    , LogEntry
    , Parent(..)
    , Player
    , Progress
    , agiCompute
    , aiDefense
    , alignmentsOf
    , applyAction
    , applyIncome
    , attackStrength
    , bestValueSpend
    , canAttack
    , cardOf
    , checkVictory
    , createGame
    , crownMoney
    , current
    , factionOf
    , findInstance
    , freeParents
    , getPlayer
    , goalProgress
    , groupCount
    , kindKey
    , leakCost
    , legalActions
    , longPlanCompute
    , maxAttackSpend
    , ownCard
    , pressureOn
    , publicGoal
    , socketsUsed
    , spendWeight
    , suggestSpend
    , totalMoney
    , twoD6Prob
    , victory
    , winReason
    , withRolls
    , yearLabel
    )

import Array
import Data exposing (Card, cardById, eventById, events, factionById, factions)
import Rng


agiCompute : Int
agiCompute =
    20


spendWeight : Int
spendWeight =
    2


longPlanCompute : Int
longPlanCompute =
    24


crownMoney : Int
crownMoney =
    64


publicGoal : Int
publicGoal =
    6


maxAttackSpend : Int
maxAttackSpend =
    4


aimProbability : Float
aimProbability =
    0.28


contestedPenalty : Int
contestedPenalty =
    2


leakNeed : Int
leakNeed =
    10


leakCost : Int
leakCost =
    4


alignmentAmount : Int
alignmentAmount =
    3


type Agent
    = Human
    | Rival
    | Reasonable


type Parent
    = Patron
    | Under String


type Attacker
    = PatronAtk
    | GroupAtk String


type Kind
    = Control
    | Neutralize
    | Destroy


type Action
    = Pass
    | Attack Kind Attacker String Int (Maybe Parent)
    | Drop String
    | Reorg String Parent
    | Leak
    | Lean String


type alias Group =
    { instanceId : String
    , cardId : String
    , money : Int
    , ownerId : Maybe Int
    , parent : Maybe Parent
    , exhausted : Bool
    , privilegedTurns : Int
    , powerMod : Int
    , powerModTurns : Int
    , extraAlignments : List String
    , disabledTurns : Int
    }


type alias Player =
    { id : Int
    , factionId : String
    , name : String
    , isAI : Bool
    , agent : Agent
    , money : Int
    , compute : Int
    , grove : List Group
    , exhausted : Bool
    , attackedThisTurn : Bool
    , capturedFrontier : List String
    , leaked : Bool
    , controlledThisTurn : Bool
    , pressure : List ( String, Int )
    , statsControlled : Int
    , statsFailed : Int
    , statsIncome : Int
    }


type alias Flags =
    { weightsLeak : Bool
    , gpuShortage : Int
    , exportControls : Int
    , visaFreeze : Int
    , safetyLetter : Int
    }


type alias Effect =
    { kind : String
    , playerId : Int
    , turns : Int
    }


type alias LogEntry =
    { when : String
    , text : String
    , kind : Maybe String
    , success : Maybe Bool
    }


type alias EventView =
    { id : String
    , name : String
    , flavor : String
    , when : String
    }


type alias Dice =
    { a : Int
    , b : Int
    , sum : Int
    }


type alias Breakdown =
    { strength : Int
    , atkP : Int
    , transfer : Int
    , spend : Int
    , aMod : Int
    , special : Int
    , res : Int
    , defSpend : Int
    , defT : Int
    , lean : Int
    , prob : Float
    }


type alias Game =
    { seed : Int
    , rng : Int
    , nextId : Int
    , scriptedRolls : List ( Int, Int )
    , year : Int
    , quarter : Int
    , round : Int
    , turnNumber : Int
    , currentPlayer : Int
    , actionsLeft : Int
    , players : List Player
    , uncontrolled : List Group
    , groupDeck : List Group
    , eventDeck : List String
    , eventDiscard : List String
    , log : List LogEntry
    , winner : Maybe Int
    , winReason : Maybe String
    , lastEvent : Maybe EventView
    , lastRoll : Maybe DiceRoll
    , flags : Flags
    , effects : List Effect
    , gameOver : Bool
    , firstPlayerOfRound : Int
    }


type alias DiceRoll =
    { a : Int
    , b : Int
    , sum : Int
    , strength : Int
    , success : Bool
    , kind : String
    , targetName : String
    , attackerName : String
    }


type alias ActResult =
    { ok : Bool
    , error : Maybe String
    , game : Game
    }


type alias Progress =
    { ratio : Float
    , publicRatio : Float
    , secretRatio : Float
    , secretText : String
    , publicText : String
    , publicDone : Bool
    }


type alias Options =
    { seed : Int
    , playerFaction : Maybe String
    , numAI : Int
    , playerName : String
    }



-- Setup


createGame : Options -> Game
createGame opts =
    let
        ( humanId, rng1 ) =
            case opts.playerFaction of
                Just id ->
                    if List.any (\f -> f.id == id) factions then
                        ( id, opts.seed )

                    else
                        pickId opts.seed

                Nothing ->
                    pickId opts.seed

        ( rest, rng2 ) =
            shuffle (List.filter (\id -> id /= humanId) (List.map .id factions)) rng1

        aiIds =
            List.take (clamp 2 3 opts.numAI) rest

        players =
            makePlayer 0 humanId False Human opts.playerName
                :: List.indexedMap
                    (\i fid ->
                        makePlayer (i + 1) fid True Rival ((factionById fid).short ++ " (AI)")
                    )
                    aiIds

        ( made, nextId ) =
            List.foldl
                (\card ( acc, n ) ->
                    ( acc ++ [ blankGroup n card.id ], n + 1 )
                )
                ( [], 1 )
                Data.cards

        ( deck0, rng3 ) =
            shuffle made rng2

        ( deck, wild ) =
            popMany 6 deck0

        ( evDeck, rng4 ) =
            shuffle (List.map .id events) rng3

        base =
            { seed = opts.seed
            , rng = rng4
            , nextId = nextId
            , scriptedRolls = []
            , year = 2026
            , quarter = 1
            , round = 1
            , turnNumber = 1
            , currentPlayer = 0
            , actionsLeft = 2
            , players = players
            , uncontrolled = wild
            , groupDeck = deck
            , eventDeck = evDeck
            , eventDiscard = []
            , log = []
            , winner = Nothing
            , winReason = Nothing
            , lastEvent = Nothing
            , lastRoll = Nothing
            , flags = emptyFlags
            , effects = []
            , gameOver = False
            , firstPlayerOfRound = 0
            }

        faction =
            factionById humanId
    in
    base
        |> say ("The window opens in 2026. " ++ String.fromInt (List.length players) ++ " patrons sit down. Only one of them is you.") Nothing Nothing
        |> say ("You are " ++ faction.name ++ ". " ++ faction.motto) Nothing Nothing
        |> (\g -> List.foldl (\p g2 -> if p.isAI then addLog g2 ("A rival takes the chair of " ++ (factionById p.factionId).name ++ ".") Nothing Nothing else g2) g players)
        |> (\g -> applyIncome g 0)
        |> drawEvent


pickId : Int -> ( String, Int )
pickId rng =
    case pick (List.map .id factions) rng of
        ( Just id, r ) ->
            ( id, r )

        ( Nothing, r ) ->
            ( "helios", r )


makePlayer : Int -> String -> Bool -> Agent -> String -> Player
makePlayer id factionId isAI agent name =
    { id = id
    , factionId = factionId
    , name = name
    , isAI = isAI
    , agent = agent
    , money = 12
    , compute = 0
    , grove = []
    , exhausted = False
    , attackedThisTurn = False
    , capturedFrontier = []
    , leaked = False
    , controlledThisTurn = False
    , pressure = []
    , statsControlled = 0
    , statsFailed = 0
    , statsIncome = 0
    }


blankGroup : Int -> String -> Group
blankGroup n cardId =
    { instanceId = "g" ++ String.fromInt n
    , cardId = cardId
    , money = 0
    , ownerId = Nothing
    , parent = Nothing
    , exhausted = False
    , privilegedTurns = 0
    , powerMod = 0
    , powerModTurns = 0
    , extraAlignments = []
    , disabledTurns = 0
    }


emptyFlags : Flags
emptyFlags =
    { weightsLeak = False, gpuShortage = 0, exportControls = 0, visaFreeze = 0, safetyLetter = 0 }


withRolls : List ( Int, Int ) -> Game -> Game
withRolls rolls game =
    { game | scriptedRolls = rolls }


ownCard : Game -> Int -> String -> Parent -> Game
ownCard game playerId cardId parent =
    case takeCard cardId game of
        ( Just g, game1 ) ->
            let
                placed =
                    { g | ownerId = Just playerId, parent = Just parent, privilegedTurns = 0, exhausted = False }
            in
            updatePlayer playerId (\p -> { p | grove = p.grove ++ [ placed ] }) game1

        ( Nothing, _ ) ->
            game


takeCard : String -> Game -> ( Maybe Group, Game )
takeCard cardId game =
    let
        pull xs =
            case List.filter (\g -> g.cardId == cardId) xs of
                g :: _ ->
                    ( Just g, List.filter (\x -> x.instanceId /= g.instanceId) xs )

                [] ->
                    ( Nothing, xs )

        ( fromWild, wild ) =
            pull game.uncontrolled

        ( fromDeck, deck ) =
            pull game.groupDeck
    in
    case fromWild of
        Just g ->
            ( Just g, { game | uncontrolled = wild } )

        Nothing ->
            case fromDeck of
                Just g ->
                    ( Just g, { game | groupDeck = deck } )

                Nothing ->
                    pullGroves cardId game


pullGroves : String -> Game -> ( Maybe Group, Game )
pullGroves cardId game =
    let
        step p ( found, ps ) =
            case found of
                Just _ ->
                    ( found, ps ++ [ p ] )

                Nothing ->
                    case List.filter (\g -> g.cardId == cardId) p.grove of
                        g :: _ ->
                            ( Just g, ps ++ [ { p | grove = List.filter (\x -> x.instanceId /= g.instanceId) p.grove } ] )

                        [] ->
                            ( Nothing, ps ++ [ p ] )

        ( hit, players ) =
            List.foldl step ( Nothing, [] ) game.players
    in
    ( hit, { game | players = players } )



-- Lookups


cardOf : Group -> Card
cardOf g =
    cardById g.cardId


factionOf : Player -> Data.Faction
factionOf p =
    factionById p.factionId


getPlayer : Game -> Int -> Player
getPlayer game id =
    case List.filter (\p -> p.id == id) game.players of
        p :: _ ->
            p

        [] ->
            makePlayer id "helios" True Rival "?"


current : Game -> Player
current game =
    getPlayer game game.currentPlayer


findInstance : Game -> String -> Maybe Group
findInstance game iid =
    let
        pools =
            game.uncontrolled ++ List.concatMap .grove game.players
    in
    List.filter (\g -> g.instanceId == iid) pools |> List.head


yearLabel : Game -> String
yearLabel game =
    let
        q =
            case game.quarter of
                1 ->
                    "Q1"

                2 ->
                    "Q2"

                3 ->
                    "Q3"

                _ ->
                    "Q4"
    in
    String.fromInt game.year ++ " " ++ q


groupCount : Player -> Int
groupCount p =
    List.length p.grove


totalMoney : Player -> Int
totalMoney p =
    p.money + List.foldl (\g s -> s + g.money) 0 p.grove


kindKey : Kind -> String
kindKey kind =
    case kind of
        Control ->
            "control"

        Neutralize ->
            "neutralize"

        Destroy ->
            "destroy"


alignmentsOf : Group -> List String
alignmentsOf g =
    unique ((cardOf g).alignments ++ g.extraAlignments)


childrenOf : Player -> Parent -> List Group
childrenOf player parent =
    List.filter (\g -> g.parent == Just parent) player.grove


socketsUsed : Player -> Parent -> Int
socketsUsed player parent =
    List.length (childrenOf player parent)


socketsMax : Player -> Parent -> Int
socketsMax player parent =
    case parent of
        Patron ->
            (factionOf player).sockets

        Under iid ->
            case List.filter (\g -> g.instanceId == iid) player.grove of
                g :: _ ->
                    (cardOf g).sockets

                [] ->
                    0


hasFree : Player -> Parent -> Bool
hasFree player parent =
    socketsUsed player parent < socketsMax player parent


freeParents : Player -> List Parent
freeParents player =
    let
        patron =
            if hasFree player Patron then
                [ Patron ]

            else
                []

        groups =
            List.filterMap
                (\g ->
                    if hasFree player (Under g.instanceId) then
                        Just (Under g.instanceId)

                    else
                        Nothing
                )
                player.grove
    in
    patron ++ groups


subtree : Player -> String -> List Group
subtree player iid =
    case List.filter (\g -> g.instanceId == iid) player.grove of
        g :: _ ->
            g :: List.concatMap (\c -> subtree player c.instanceId) (childrenOf player (Under iid))

        [] ->
            []


groupPower : Group -> Int
groupPower g =
    let
        p =
            (cardOf g).power + g.powerMod
    in
    if g.disabledTurns > 0 then
        max 0 (p - 2)

    else
        p


resistanceOf : Group -> Int
resistanceOf g =
    if g.disabledTurns > 0 then
        max 1 ((cardOf g).resistance - 1)

    else
        (cardOf g).resistance


effectiveIncome : Player -> Group -> Int
effectiveIncome player g =
    if g.privilegedTurns > 0 then
        0

    else
        let
            card =
                cardOf g

            bumped =
                if player.factionId == "long-plan" && card.type_ == "chipmaker" then
                    card.income + 1

                else if player.factionId == "crown" && (card.type_ == "power" || card.type_ == "datacenter") then
                    card.income + 1

                else
                    card.income

            hasQueue =
                List.any (\x -> x.cardId == "interconnect") player.grove
        in
        if hasQueue && card.type_ == "power" && card.id /= "interconnect" then
            bumped + 1

        else
            bumped


effectiveCompute : Game -> Group -> Int
effectiveCompute game g =
    let
        -- Helios labs and chipmakers start paying compute one turn early
        -- (privilege counter 1). Money and attack immunity still wait out four.
        heliosEarly =
            g.privilegedTurns == 1 && heliosLabOrChip game g
    in
    if game.flags.gpuShortage > 0 || (g.privilegedTurns > 0 && not heliosEarly) then
        0

    else
        let
            c =
                (cardOf g).compute
        in
        if g.disabledTurns > 0 then
            max 0 (c // 2)

        else
            c


heliosLabOrChip : Game -> Group -> Bool
heliosLabOrChip game g =
    case g.ownerId of
        Just oid ->
            let
                owner =
                    getPlayer game oid

                kind =
                    (cardOf g).type_
            in
            owner.factionId == "helios" && (kind == "lab" || kind == "chipmaker")

        Nothing ->
            False


twoD6Prob : Int -> Float
twoD6Prob need =
    if need < 2 then
        0

    else
        let
            maxS =
                min need 11

            ways =
                List.sum
                    (List.map
                        (\a ->
                            List.sum
                                (List.map
                                    (\b ->
                                        let
                                            s =
                                                a + b
                                        in
                                        if s <= maxS && s /= 12 then
                                            1

                                        else
                                            0
                                    )
                                    (List.range 1 6)
                                )
                        )
                        (List.range 1 6)
                    )
        in
        toFloat ways / 36



-- Income, events, turns


applyIncome : Game -> Int -> Game
applyIncome game pid =
    if game.gameOver then
        game

    else
        let
            p =
                getPlayer game pid

            f =
                factionOf p

            ( grove2, groupInc, comp ) =
                List.foldl
                    (\g ( gs, inc, c ) ->
                        let
                            add =
                                effectiveIncome p g
                        in
                        ( gs ++ [ { g | money = g.money + add } ], inc + add, c + effectiveCompute game g )
                    )
                    ( [], 0, 0 )
                    p.grove

            comp2 =
                List.foldl
                    (\e acc ->
                        if e.kind == "plant-delay" && e.playerId == pid then
                            max 0 (acc - 2)

                        else
                            acc
                    )
                    comp
                    game.effects

            gained =
                f.income + groupInc

            p2 =
                { p
                    | money = p.money + f.income
                    , grove = grove2
                    , compute = p.compute + comp2
                    , statsIncome = p.statsIncome + gained
                }
        in
        addLog (replacePlayer game p2)
            (p2.name
                ++ " "
                ++ agrees p2.name "collects"
                ++ " "
                ++ String.fromInt gained
                ++ " money"
                ++ (if comp2 /= 0 then
                        " and " ++ String.fromInt comp2 ++ " compute"

                    else
                        ""
                   )
                ++ ". Treasury "
                ++ String.fromInt p2.money
                ++ ", compute "
                ++ String.fromInt p2.compute
                ++ "."
            )
            Nothing
            Nothing


drawEvent : Game -> Game
drawEvent game =
    if game.gameOver then
        game

    else
        let
            ( deck, discard, rng ) =
                if List.isEmpty game.eventDeck then
                    let
                        ( shuffled, r ) =
                            shuffle game.eventDiscard game.rng
                    in
                    ( shuffled, [], r )

                else
                    ( game.eventDeck, game.eventDiscard, game.rng )
        in
        case popEnd deck of
            Nothing ->
                { game | rng = rng, eventDeck = deck, eventDiscard = discard }

            Just ( id, rest ) ->
                let
                    ev =
                        eventById id

                    game1 =
                        { game
                            | rng = rng
                            , eventDeck = rest
                            , eventDiscard = discard ++ [ id ]
                            , lastEvent = Just { id = id, name = ev.name, flavor = ev.flavor, when = yearLabel game }
                        }
                in
                game1
                    |> say ("Headline: " ++ ev.name ++ ". " ++ ev.flavor) (Just "event") Nothing
                    |> (\g -> resolveEvent g id)


resolveEvent : Game -> String -> Game
resolveEvent game id =
    let
        actor =
            current game
    in
    case id of
        "plant-delay" ->
            let
                ranked =
                    List.sortWith
                        (\a b ->
                            case compare (powerCount b) (powerCount a) of
                                EQ ->
                                    compare b.compute a.compute

                                other ->
                                    other
                        )
                        game.players
            in
            case ranked of
                t :: _ ->
                    let
                        loss =
                            min 2 t.compute

                        game1 =
                            updatePlayer t.id (\p -> { p | compute = p.compute - loss }) game
                                |> (\g -> { g | effects = g.effects ++ [ { kind = "plant-delay", playerId = t.id, turns = 1 } ] })
                    in
                    addLog game1 (getPlayer game1 t.id |> .name |> \n -> n ++ " " ++ agrees n "is" ++ " hit by a plant delay (" ++ String.fromInt loss ++ " compute lost).") Nothing Nothing

                [] ->
                    game

        "export-controls" ->
            addLog { game | flags = setExport 2 game.flags } "Export controls tighten for two rounds. Chipmakers not held by The Long Plan lose 2 power." Nothing Nothing

        "gpu-shortage" ->
            addLog { game | flags = setGpu 1 game.flags } "A GPU shortage: no compute will be produced until the next round." Nothing Nothing

        "boardroom-coup" ->
            let
                founders =
                    List.concatMap
                        (\p ->
                            List.filterMap
                                (\g ->
                                    if List.member "Founder" (cardOf g).traits then
                                        Just ( p.id, g )

                                    else
                                        Nothing
                                )
                                p.grove
                        )
                        game.players

                wild =
                    List.filter (\g -> List.member "Founder" (cardOf g).traits) game.uncontrolled
            in
            if not (List.isEmpty founders) then
                let
                    ( picked, game1 ) =
                        pickGame game founders
                in
                case picked of
                    Just ( pid, g ) ->
                        let
                            owner =
                                getPlayer game1 pid
                        in
                        game1
                            |> say ("A boardroom coup ousts " ++ (cardOf g).name ++ " from " ++ owner.name ++ "'s structure.") Nothing Nothing
                            |> (\g2 -> neutralize g2 pid g.instanceId True)

                    Nothing ->
                        game1

            else if not (List.isEmpty wild) then
                let
                    ( picked, game1 ) =
                        pickGame game wild
                in
                case picked of
                    Just g ->
                        addLog (mapInstance g.instanceId (\x -> { x | powerMod = -2, powerModTurns = 2 }) game1) ((cardOf g).name ++ " is unsettled by a coup attempt (−2 power).") Nothing Nothing

                    Nothing ->
                        game1

            else
                addLog game "The coup memo circulates, but no founder is sitting in a chair that can be pulled." Nothing Nothing

        "weights-leak" ->
            let
                labs =
                    List.filter (\g -> (cardOf g).type_ == "lab") (allInstances game)

                ( picked, game1 ) =
                    pickGame game labs

                game2 =
                    case picked of
                        Just g ->
                            addLog (mapInstance g.instanceId addOpen game1) ("Weights leak from " ++ (cardOf g).name ++ ". It picks up the Open alignment. The files are out.") Nothing Nothing

                        Nothing ->
                            addLog game1 "Weights leak from a lab no one controls. The files are out anyway." Nothing Nothing
            in
            markLeaked { game2 | flags = setLeak True game2.flags }

        "cluster-fire" ->
            let
                ranked =
                    List.sortWith (\a b -> compare b.compute a.compute) game.players
            in
            case ranked of
                t :: _ ->
                    let
                        loss =
                            min 3 t.compute
                    in
                    addLog (updatePlayer t.id (\p -> { p | compute = p.compute - loss }) game) ("A cluster fire burns " ++ String.fromInt loss ++ " compute off " ++ t.name ++ ".") Nothing Nothing

                [] ->
                    game

        "talent-exodus" ->
            let
                labs =
                    List.filter (\g -> (cardOf g).type_ == "lab" && g.ownerId /= Nothing) (allInstances game)

                ( picked, game1 ) =
                    pickGame game labs
            in
            case picked of
                Just g ->
                    addLog (mapInstance g.instanceId (\x -> { x | powerMod = x.powerMod - 2, powerModTurns = 2 }) game1) ("Talent walks out of " ++ (cardOf g).name ++ " (−2 power for two turns).") Nothing Nothing

                Nothing ->
                    addLog game1 "The poaching wave finds no lab worth raiding." Nothing Nothing

        "safety-letter" ->
            case findFaction game "pause" of
                Just pause ->
                    addLog (updatePlayer pause.id (\p -> { p | money = p.money + 3 }) { game | flags = setSafety 2 game.flags }) ("The Safety Letter lands. " ++ pause.name ++ " " ++ agrees pause.name "raises" ++ " 3 money. Accelerationist groups are under pressure.") Nothing Nothing

                Nothing ->
                    addLog { game | flags = setSafety 2 game.flags } "The Safety Letter lands. Accelerationist groups are under pressure for two rounds." Nothing Nothing

        "funding-round" ->
            let
                ( game1, any ) =
                    List.foldl
                        (\p ( g, hit ) ->
                            let
                                n =
                                    List.length (List.filter (\grp -> (cardOf grp).type_ == "vc") p.grove)
                            in
                            if n > 0 then
                                let
                                    gain =
                                        4 * n

                                    fresh =
                                        getPlayer g p.id
                                in
                                ( addLog (updatePlayer p.id (\q -> { q | money = q.money + gain }) g) (fresh.name ++ " " ++ agrees fresh.name "rides" ++ " a funding round for +" ++ String.fromInt gain ++ ".") Nothing Nothing, True )

                            else
                                ( g, hit )
                        )
                        ( game, False )
                        game.players
            in
            if any then
                game1

            else
                addLog (updatePlayer actor.id (\p -> { p | money = p.money + 2 }) game1) ("A general funding wave: " ++ actor.name ++ " " ++ agrees actor.name "picks" ++ " up 2 money.") Nothing Nothing

        "hearing" ->
            let
                labs =
                    List.filter (\g -> List.member "Frontier" (cardOf g).traits && g.ownerId /= Nothing) (allInstances game)

                ( picked, game1 ) =
                    pickGame game labs
            in
            case picked of
                Just g ->
                    case g.ownerId of
                        Just oid ->
                            let
                                owner =
                                    getPlayer game1 oid
                            in
                            addLog (mapInstance g.instanceId (\x -> { x | exhausted = True, disabledTurns = max x.disabledTurns 1 }) game1) ((cardOf g).name ++ " (" ++ owner.name ++ ") is called to a hearing and cannot act this turn.") Nothing Nothing

                        Nothing ->
                            game1

                Nothing ->
                    addLog game1 "The hearing is scheduled, then postponed. No lab is available to swear in." Nothing Nothing

        "eval-scoop" ->
            let
                labs =
                    List.filter (\g -> (cardOf g).type_ == "lab" && g.ownerId /= Nothing) (allInstances game)

                ( picked, game1 ) =
                    pickGame game labs
            in
            case picked of
                Just g ->
                    case g.ownerId of
                        Just oid ->
                            let
                                owner =
                                    getPlayer game1 oid
                            in
                            addLog (updatePlayer oid (\p -> { p | compute = p.compute + 2 }) game1) ("An eval scoop: " ++ (cardOf g).name ++ " hands " ++ owner.name ++ " +2 compute.") Nothing Nothing

                        Nothing ->
                            game1

                Nothing ->
                    addLog (updatePlayer actor.id (\p -> { p | compute = p.compute + 1 }) game1) ("A public eval leaks. " ++ actor.name ++ " " ++ agrees actor.name "skims" ++ " +1 compute.") Nothing Nothing

        "interconnect-denial" ->
            let
                dcs =
                    List.filter (\g -> (cardOf g).type_ == "datacenter" && g.ownerId /= Nothing) (allInstances game)

                ( picked, game1 ) =
                    pickGame game dcs
            in
            case picked of
                Just g ->
                    case g.ownerId of
                        Just oid ->
                            let
                                owner =
                                    getPlayer game1 oid

                                loss =
                                    min 2 owner.compute
                            in
                            addLog (mapInstance g.instanceId (\x -> { x | disabledTurns = max x.disabledTurns 1 }) (updatePlayer oid (\p -> { p | compute = p.compute - loss }) game1)) ("Interconnect denied at " ++ (cardOf g).name ++ ": " ++ owner.name ++ " loses " ++ String.fromInt loss ++ " compute.") Nothing Nothing

                        Nothing ->
                            game1

                Nothing ->
                    addLog game1 "A utility denies an interconnect, but no campus is far enough along to care." Nothing Nothing

        "open-drop" ->
            let
                game1 =
                    case List.filter (\g -> g.cardId == "open-weights") (allInstances game) of
                        g :: _ ->
                            addLog (mapInstance g.instanceId (\x -> { x | powerMod = x.powerMod + 2, powerModTurns = 3 }) game) "An open-source drop empowers the Open Weights Collective (+2 power)." Nothing Nothing

                        [] ->
                            game
            in
            addLog (markLeaked { game1 | flags = setLeak True game1.flags }) "The drop is treated as a Weights Leak." Nothing Nothing

        "emergency-powers" ->
            case findFaction game "directorate" of
                Just d ->
                    if d.id == actor.id then
                        addLog { game | actionsLeft = game.actionsLeft + 1 } (d.name ++ " " ++ agrees d.name "invokes" ++ " emergency powers and gains an extra action.") Nothing Nothing

                    else
                        addLog (updatePlayer d.id (\p -> { p | money = p.money + 3 }) game) (d.name ++ " " ++ agrees d.name "banks" ++ " 3 money from emergency appropriations.") Nothing Nothing

                Nothing ->
                    addLog (updatePlayer actor.id (\p -> { p | money = p.money + 2 }) game) (actor.name ++ " " ++ agrees actor.name "skims" ++ " 2 money off an emergency appropriations bill.") Nothing Nothing

        "subsidy" ->
            let
                t =
                    case findFaction game "crown" of
                        Just c ->
                            c

                        Nothing ->
                            case List.filter (\p -> List.any (\g -> (cardOf g).type_ == "power") p.grove) game.players of
                                p :: _ ->
                                    p

                                [] ->
                                    actor
            in
            addLog (updatePlayer t.id (\p -> { p | money = p.money + 5 }) game) (t.name ++ " " ++ agrees t.name "receives" ++ " a 5-money industrial subsidy.") Nothing Nothing

        "flash-crash" ->
            let
                ranked =
                    List.sortWith (\a b -> compare (totalMoney b) (totalMoney a)) game.players
            in
            case ranked of
                t :: _ ->
                    let
                        loss =
                            min 4 t.money
                    in
                    addLog (updatePlayer t.id (\p -> { p | money = p.money - loss }) game) ("A flash crash takes " ++ String.fromInt loss ++ " money from " ++ t.name ++ ".") Nothing Nothing

                [] ->
                    game

        "demo-day" ->
            let
                labs =
                    List.filter (\g -> (cardOf g).type_ == "lab") (allInstances game)

                ( picked, game1 ) =
                    pickGame game labs
            in
            case picked of
                Just g ->
                    addLog (mapInstance g.instanceId (\x -> { x | powerMod = x.powerMod + 3, powerModTurns = 1 }) game1) ((cardOf g).name ++ " has a stunning demo (+3 power this round).") Nothing Nothing

                Nothing ->
                    addLog game1 "Demo day is postponed: no lab is on the program." Nothing Nothing

        "visa-freeze" ->
            addLog { game | flags = setVisa 1 game.flags } "A visa freeze: Talent groups cannot lead attacks this round." Nothing Nothing

        "good-news" ->
            addLog (updatePlayer actor.id (\p -> { p | money = p.money + 1 }) game) ("A quiet quarter. " ++ actor.name ++ " " ++ agrees actor.name "pockets" ++ " a modest 1 money.") Nothing Nothing

        "grid-upgrade" ->
            let
                pool =
                    List.filter (\p -> List.any (\g -> (cardOf g).type_ == "power" || (cardOf g).type_ == "datacenter") p.grove) game.players

                ( picked, game1 ) =
                    if List.isEmpty pool then
                        ( Just actor, game )

                    else
                        pickGame game pool
            in
            case picked of
                Just t ->
                    addLog (updatePlayer t.id (\p -> { p | compute = p.compute + 2, money = p.money + 1 }) game1) ("A grid upgrade lands for " ++ t.name ++ ": +2 compute, +1 money.") Nothing Nothing

                Nothing ->
                    game1

        _ ->
            addLog game "The headline fades without a clear winner." Nothing Nothing


endTurn : Game -> Game
endTurn game =
    if game.gameOver then
        game

    else
        let
            game1 =
                checkVictory game
        in
        if game1.gameOver then
            game1

        else
            let
                pid =
                    game1.currentPlayer

                game2 =
                    updatePlayer pid
                        (\p ->
                            { p
                                | exhausted = False
                                , controlledThisTurn = False
                                , attackedThisTurn = False
                                , grove =
                                    List.map
                                        (\g ->
                                            { g
                                                | exhausted = False
                                                , privilegedTurns =
                                                    if g.privilegedTurns > 0 then
                                                        g.privilegedTurns - 1

                                                    else
                                                        0
                                            }
                                        )
                                        p.grove
                            }
                        )
                        game1

                next =
                    modBy (List.length game2.players) (pid + 1)

                wrapped =
                    next == game2.firstPlayerOfRound

                game3 =
                    if not wrapped then
                        game2

                    else
                        let
                            advanced =
                                bumpQuarter { game2 | round = (tick game2).round + 1 }
                                    |> (\g -> { g | flags = (tick game2).flags, effects = (tick game2).effects, players = (tick game2).players, rng = (tick game2).rng })
                        in
                        if advanced.round > 20 then
                            closeWindow advanced

                        else
                            addLog advanced ("— " ++ yearLabel advanced ++ " begins. —") (Just "time") Nothing
            in
            if game3.gameOver then
                game3

            else
                let
                    game4 =
                        { game3
                            | currentPlayer = next
                            , turnNumber = game3.turnNumber + 1
                            , actionsLeft = 2
                        }

                    game5 =
                        applyIncome game4 next

                    game6 =
                        if wrapped then
                            drawEvent game5

                        else
                            game5
                in
                checkVictory game6


tick : Game -> Game
tick game =
    { game
        | flags =
            { weightsLeak = game.flags.weightsLeak
            , gpuShortage = dec game.flags.gpuShortage
            , exportControls = dec game.flags.exportControls
            , visaFreeze = dec game.flags.visaFreeze
            , safetyLetter = dec game.flags.safetyLetter
            }
        , effects =
            List.filterMap
                (\e ->
                    if e.turns - 1 > 0 then
                        Just { e | turns = e.turns - 1 }

                    else
                        Nothing
                )
                game.effects
        , players = List.map tickPlayer game.players
    }


tickPlayer : Player -> Player
tickPlayer p =
    { p
        | grove =
            List.map
                (\g ->
                    let
                        turns =
                            if g.powerModTurns > 0 then
                                g.powerModTurns - 1

                            else
                                0
                    in
                    { g
                        | powerModTurns = turns
                        , powerMod =
                            if g.powerModTurns > 0 && turns <= 0 then
                                0

                            else
                                g.powerMod
                        , disabledTurns =
                            if g.disabledTurns > 0 then
                                g.disabledTurns - 1

                            else
                                0
                    }
                )
                p.grove
    }


bumpQuarter : Game -> Game
bumpQuarter g =
    let
        q =
            g.quarter + 1
    in
    if q > 4 then
        { g | quarter = 1, year = g.year + 1 }

    else
        { g | quarter = q }


closeWindow : Game -> Game
closeWindow game =
    let
        game1 =
            addLog game "2031 arrives. The window closes. No one claimed a clean win — the closest patron is scored." Nothing Nothing

        scored =
            List.map (\p -> ( p, scorePlayer game1 p )) game1.players

        game2 =
            List.foldl (\( p, s ) g -> addLog g (p.name ++ " " ++ agrees p.name "scores" ++ " " ++ String.fromInt s ++ ".") Nothing Nothing) game1 scored

        bestScore =
            List.foldl (\( _, s ) m -> max s m) -999999 scored

        tied =
            List.filterMap
                (\( p, s ) ->
                    if s == bestScore then
                        Just p

                    else
                        Nothing
                )
                scored

        best =
            case List.sortBy (\p -> ( -p.compute, -(totalMoney p), p.id )) tied of
                p :: _ ->
                    p

                [] ->
                    current game2

        reason =
            "The window closed in 2031. "
                ++ (if best.name == "You" then
                        "You are closest to takeoff (score " ++ String.fromInt bestScore ++ ")."

                    else
                        best.name ++ " is closest to takeoff (score " ++ String.fromInt bestScore ++ ")."
                   )
    in
    addLog { game2 | winner = Just best.id, winReason = Just reason, gameOver = True } reason (Just "win") Nothing


scorePlayer : Game -> Player -> Int
scorePlayer game p =
    let
        prog =
            goalProgress game p
    in
    groupCount p * 4 + p.compute * 2 + (totalMoney p // 3) + floor (prog.ratio * 30)


spendAction : Game -> Game
spendAction game =
    if game.gameOver then
        game

    else
        let
            left =
                max 0 (game.actionsLeft - 1)

            game1 =
                { game | actionsLeft = left }
        in
        if left <= 0 then
            endTurn game1

        else
            game1


checkVictory : Game -> Game
checkVictory game =
    if game.gameOver then
        game

    else
        let
            cur =
                current game

            order =
                cur :: List.filter (\p -> p.id /= cur.id) game.players
        in
        case firstJust (\p -> Maybe.map (\r -> ( p, r )) (winReason game p)) order of
            Just ( p, reason ) ->
                addLog { game | winner = Just p.id, winReason = Just reason, gameOver = True } reason (Just "win") Nothing

            Nothing ->
                game



-- Victory


victory : Game -> Int -> Maybe String
victory game pid =
    winReason game (getPlayer game pid)


winReason : Game -> Player -> Maybe String
winReason game p =
    let
        all =
            p.grove

        mature =
            List.filter (\g -> g.privilegedTurns <= 0) all

        n =
            List.length mature

        who =
            if p.isAI then
                (factionOf p).name ++ " (AI)"

            else if p.name == "You" then
                "You (" ++ (factionOf p).name ++ ")"

            else
                p.name ++ " (" ++ (factionOf p).name ++ ")"

        verb =
            if (not p.isAI) && p.name == "You" then
                "win"

            else
                "wins"

        types t =
            List.filter (\g -> (cardOf g).type_ == t) mature

        aligns a =
            List.filter (\g -> List.member a (alignmentsOf g)) mature

        has card trait =
            List.member trait card.traits
    in
    if n >= publicGoal then
        Just (who ++ " " ++ verb ++ " by public influence: " ++ String.fromInt n ++ " groups under their structure.")

    else if p.factionId == "helios" && p.compute >= agiCompute then
        Just (who ++ " " ++ (if verb == "win" then "reach" else "reaches") ++ " takeoff: " ++ String.fromInt p.compute ++ " compute, enough to train the frontier model.")

    else if p.factionId == "helios" && p.compute >= 12 && List.any (\g -> (cardOf g).type_ == "lab") mature && List.any (\g -> (cardOf g).type_ == "chipmaker") mature then
        Just (who ++ " " ++ verb ++ " by fielding a lab and a chipmaker with " ++ String.fromInt p.compute ++ " compute.")

    else if p.factionId == "open-wave" && game.flags.weightsLeak && List.length (aligns "Open") >= 3 then
        Just (who ++ " " ++ verb ++ ": the weights are out, and they control the Open stack.")

    else if p.factionId == "open-wave" && List.any (\g -> g.cardId == "open-weights") mature && n >= 4 then
        Just (who ++ " " ++ verb ++ " by making the Open Weights Collective the center of a four-group network.")

    else if p.factionId == "pause" && List.length p.capturedFrontier >= 3 then
        Just (who ++ " " ++ verb ++ ": three Frontier labs have been pulled off the board.")

    else if p.factionId == "pause" && not (List.isEmpty (types "regulator")) && not (List.isEmpty (types "media")) && n >= 3 then
        Just (who ++ " " ++ verb ++ " by owning the referee, the headline, and a third proxy.")

    else if p.factionId == "directorate" && List.any (\g -> has (cardOf g) "Frontier") mature && not (List.isEmpty (types "regulator")) && n >= 3 then
        Just (who ++ " " ++ verb ++ ": a Frontier lab, a regulator, and a three-group stack sit in the same structure.")

    else if p.factionId == "directorate" && n >= 4 && List.length (aligns "Nationalist") >= 2 then
        Just (who ++ " " ++ verb ++ " with a four-group nationalist stack.")

    else if p.factionId == "long-plan" && not (List.isEmpty (types "chipmaker")) && not (List.isEmpty (types "hyperscaler")) && n >= 5 then
        Just (who ++ " " ++ verb ++ ": the fab, the cloud, and a five-group stack are theirs.")

    else if p.factionId == "long-plan" && p.compute >= longPlanCompute && List.all (\o -> o.id == p.id || p.compute > o.compute) game.players then
        Just (who ++ " " ++ verb ++ " with a sovereign compute lead (" ++ String.fromInt p.compute ++ ").")

    else if p.factionId == "crown" && not (List.isEmpty (types "power")) && not (List.isEmpty (types "datacenter")) && not (List.isEmpty (types "chipmaker")) then
        Just (who ++ " " ++ verb ++ " by owning power, concrete, and silicon.")

    else if p.factionId == "crown" && totalMoney p >= crownMoney && n >= 3 then
        Just (who ++ " " ++ verb ++ " as the liquid sovereign: " ++ String.fromInt (totalMoney p) ++ " money and " ++ String.fromInt n ++ " groups.")

    else
        Nothing


goalProgress : Game -> Player -> Progress
goalProgress game p =
    let
        all =
            p.grove

        mature =
            List.filter (\g -> g.privilegedTurns <= 0) all

        n =
            List.length mature

        nAll =
            List.length all

        types t =
            List.filter (\g -> (cardOf g).type_ == t) mature

        aligns a =
            List.filter (\g -> List.member a (alignmentsOf g)) mature

        publicRatio =
            min 1 (toFloat n / toFloat publicGoal)

        openN =
            List.length (aligns "Open")

        hasOW =
            List.any (\g -> g.cardId == "open-weights") all

        ( secretRatio, secretText ) =
            if p.factionId == "helios" then
                let
                    hasLab =
                        List.any (\g -> (cardOf g).type_ == "lab") mature

                    hasChip =
                        List.any (\g -> (cardOf g).type_ == "chipmaker") mature

                    stack =
                        if hasLab && hasChip then
                            min 1 (toFloat p.compute / 12)

                        else
                            (if hasLab then 0.25 else 0) + (if hasChip then 0.25 else 0) + min 0.3 (toFloat p.compute / 20)
                in
                ( max (min 1 (toFloat p.compute / toFloat agiCompute)) stack
                , speak p
                    ++ " "
                    ++ have p
                    ++ " "
                    ++ String.fromInt p.compute
                    ++ " compute. Helios wins at "
                    ++ String.fromInt agiCompute
                    ++ ", or with a lab, a chipmaker, and 12 compute. "
                    ++ (if hasLab then
                            speak p ++ " " ++ hold p ++ " a settled lab. "

                        else
                            speak p ++ " " ++ doNot p ++ " hold a settled lab. "
                       )
                    ++ (if hasChip then
                            speak p ++ " " ++ hold p ++ " a settled chipmaker."

                        else
                            speak p ++ " " ++ doNot p ++ " hold a settled chipmaker."
                       )
                )

            else if p.factionId == "open-wave" then
                let
                    a =
                        if game.flags.weightsLeak && openN >= 3 then
                            1

                        else
                            min 0.9 ((if game.flags.weightsLeak then 0.4 else 0) + min 0.6 (toFloat openN / 5))

                    b =
                        if hasOW && n >= 4 then
                            1

                        else
                            (if hasOW then 0.4 else 0) + min 0.55 (toFloat n / 8)
                in
                ( max a b
                , (if game.flags.weightsLeak then
                    "The weights are already out. "

                   else
                    "The weights are still sealed. "
                  )
                    ++ speak p
                    ++ " "
                    ++ hold p
                    ++ " "
                    ++ String.fromInt openN
                    ++ " of 3 Open groups. "
                    ++ (if hasOW then
                            speak p ++ " " ++ hold p ++ " the Open Weights Collective"

                        else
                            speak p ++ " " ++ doNot p ++ " hold the Open Weights Collective"
                       )
                    ++ ". "
                    ++ speak p
                    ++ " "
                    ++ hold p
                    ++ " "
                    ++ String.fromInt n
                    ++ " of 4 settled groups."
                )

            else if p.factionId == "pause" then
                let
                    paused =
                        List.length p.capturedFrontier

                    hasReg =
                        not (List.isEmpty (types "regulator"))

                    hasMedia =
                        not (List.isEmpty (types "media"))

                    a =
                        min 1 (toFloat paused / 3)

                    b =
                        (if hasReg then 0.35 else 0) + (if hasMedia then 0.35 else 0) + (if n >= 3 && hasReg && hasMedia then 0.3 else min 0.25 (toFloat n / 12))
                in
                ( max a b
                , speak p
                    ++ " "
                    ++ have p
                    ++ " removed "
                    ++ String.fromInt paused
                    ++ " of 3 Frontier groups. "
                    ++ (if hasReg then
                            speak p ++ " " ++ hold p ++ " a regulator. "

                        else
                            speak p ++ " " ++ doNot p ++ " hold a regulator. "
                       )
                    ++ (if hasMedia then
                            speak p ++ " " ++ hold p ++ " a media group. "

                        else
                            speak p ++ " " ++ doNot p ++ " hold a media group. "
                       )
                    ++ speak p
                    ++ " "
                    ++ hold p
                    ++ " "
                    ++ String.fromInt n
                    ++ " of 3 settled groups."
                )

            else if p.factionId == "directorate" then
                let
                    hasFront =
                        List.any (\g -> List.member "Frontier" (cardOf g).traits) mature

                    hasReg =
                        not (List.isEmpty (types "regulator"))

                    natN =
                        List.length (aligns "Nationalist")

                    a =
                        (if hasFront then 0.3 else 0) + (if hasReg then 0.3 else 0) + (if n >= 3 && hasFront && hasReg then 0.4 else min 0.3 (toFloat n / 10))

                    b =
                        if n >= 4 && natN >= 2 then
                            1

                        else
                            min 0.85 ((toFloat n / 4) * 0.55 + min 0.3 (toFloat natN / 2))
                in
                ( max a b
                , (if hasFront then
                    "A Frontier group is in the structure. "

                   else
                    "No Frontier group yet. "
                  )
                    ++ (if hasReg then
                            "A regulator is in the structure. "

                        else
                            "No regulator yet. "
                       )
                    ++ speak p
                    ++ " "
                    ++ hold p
                    ++ " "
                    ++ String.fromInt n
                    ++ " of 3 settled groups on that path. The other path is 4 settled groups including 2 Nationalist ones. "
                    ++ speak p
                    ++ " "
                    ++ hold p
                    ++ " "
                    ++ String.fromInt natN
                    ++ " so far."
                )

            else if p.factionId == "long-plan" then
                let
                    hasChip =
                        not (List.isEmpty (types "chipmaker"))

                    hasHyper =
                        not (List.isEmpty (types "hyperscaler"))

                    lead =
                        p.compute >= longPlanCompute && List.all (\o -> o.id == p.id || p.compute > o.compute) game.players

                    a =
                        (if hasChip then 0.25 else 0) + (if hasHyper then 0.25 else 0) + (if n >= 5 && hasChip && hasHyper then 0.5 else min 0.3 (toFloat n / 16))

                    b =
                        if lead then
                            1

                        else
                            min 0.9 (toFloat p.compute / toFloat longPlanCompute)
                in
                ( max a b
                , (if hasChip then
                    speak p ++ " " ++ hold p ++ " a chipmaker. "

                   else
                    speak p ++ " " ++ doNot p ++ " hold a chipmaker. "
                  )
                    ++ (if hasHyper then
                            speak p ++ " " ++ hold p ++ " a hyperscaler. "

                        else
                            speak p ++ " " ++ doNot p ++ " hold a hyperscaler. "
                       )
                    ++ speak p
                    ++ " "
                    ++ hold p
                    ++ " "
                    ++ String.fromInt n
                    ++ " of 5 settled groups. The compute path is "
                    ++ String.fromInt p.compute
                    ++ " of "
                    ++ String.fromInt longPlanCompute
                    ++ (if lead then
                            ", and " ++ speak p ++ " " ++ have p ++ " the lead."

                        else
                            ", still short of a strict lead."
                       )
                )

            else
                let
                    hasP =
                        not (List.isEmpty (types "power"))

                    hasD =
                        not (List.isEmpty (types "datacenter"))

                    hasC =
                        not (List.isEmpty (types "chipmaker"))

                    money =
                        totalMoney p

                    a =
                        (if hasP then 1 / 3 else 0) + (if hasD then 1 / 3 else 0) + (if hasC then 1 / 3 else 0)

                    b =
                        if money >= crownMoney && n >= 3 then
                            1

                        else
                            min 0.9 ((toFloat money / toFloat crownMoney) * 0.6 + min 0.4 (toFloat n / 9))
                in
                ( max a b
                , (if hasP then
                    "Power is secured. "

                   else
                    "No power group yet. "
                  )
                    ++ (if hasD then
                            "A datacenter is secured. "

                        else
                            "No datacenter yet. "
                       )
                    ++ (if hasC then
                            "A chipmaker is secured. "

                        else
                            "No chipmaker yet. "
                       )
                    ++ "The other path is "
                    ++ String.fromInt money
                    ++ " of "
                    ++ String.fromInt crownMoney
                    ++ " money with "
                    ++ String.fromInt n
                    ++ " of 3 settled groups."
                )

        won =
            winReason game p /= Nothing
    in
    { ratio = if won then 1 else max secretRatio publicRatio
    , publicRatio = publicRatio
    , secretRatio = if won then max 1 secretRatio else secretRatio
    , secretText = secretText
    , publicText = String.fromInt n ++ "/" ++ String.fromInt publicGoal ++ " settled groups"
    , publicDone = n >= publicGoal
    }



-- Attacks


alignmentMod : List String -> List String -> Kind -> Int
alignmentMod atk def kind =
    List.foldl
        (\a acc ->
            let
                same =
                    if List.member a def then
                        alignmentAmount

                    else
                        0

                opp =
                    case opposite a of
                        Just o ->
                            if List.member o def then
                                alignmentAmount

                            else
                                0

                        Nothing ->
                            0
            in
            if kind == Control then
                acc + same - opp

            else
                acc + opp - same
        )
        0
        atk


opposite : String -> Maybe String
opposite a =
    case a of
        "Corporate" ->
            Just "Open"

        "Open" ->
            Just "Corporate"

        "Safety" ->
            Just "Accelerationist"

        "Accelerationist" ->
            Just "Safety"

        "Nationalist" ->
            Just "Chaotic"

        "Chaotic" ->
            Just "Nationalist"

        _ ->
            Nothing


attackStrength : Game -> Int -> Attacker -> String -> Kind -> Int -> Int -> Breakdown
attackStrength game playerId attacker targetId kind spend defSpend =
    let
        player =
            getPlayer game playerId
    in
    case findInstance game targetId of
        Nothing ->
            { strength = 0, atkP = 0, transfer = 0, spend = spend, aMod = 0, special = 0, res = 0, defSpend = defSpend, defT = 0, lean = 0, prob = 0 }

        Just target ->
            let
                atkGroup =
                    case attacker of
                        GroupAtk iid ->
                            findInstance game iid

                        PatronAtk ->
                            Nothing

                atkP0 =
                    case attacker of
                        PatronAtk ->
                            (factionOf player).power

                        GroupAtk _ ->
                            Maybe.map groupPower atkGroup |> Maybe.withDefault 0

                atkP =
                    case atkGroup of
                        Just g ->
                            if game.flags.exportControls > 0 && (cardOf g).type_ == "chipmaker" && player.factionId /= "long-plan" then
                                max 0 (atkP0 - 2)

                            else
                                atkP0

                        Nothing ->
                            atkP0

                transfer =
                    transferSupport game player attacker

                aligns =
                    Maybe.map alignmentsOf atkGroup |> Maybe.withDefault []

                aMod =
                    alignmentMod aligns (alignmentsOf target) kind

                special =
                    specialMod game player attacker target kind

                res =
                    resistanceOf target

                defT =
                    defenseSupport game target

                leaned =
                    pressureOn player target.instanceId

                spendPower =
                    spend * spendWeight

                strength =
                    atkP + transfer + spendPower + leaned + aMod + special - res - defSpend - defT - contestedPenalty
            in
            { strength = strength
            , atkP = atkP
            , transfer = transfer
            , spend = spend
            , aMod = aMod
            , special = special
            , res = res
            , defSpend = defSpend
            , defT = defT
            , lean = leaned
            , prob = twoD6Prob strength
            }


transferSupport : Game -> Player -> Attacker -> Int
transferSupport game player attacker =
    case attacker of
        PatronAtk ->
            0

        GroupAtk iid ->
            case findInstance game iid of
                Nothing ->
                    0

                Just g ->
                    case g.parent of
                        Just Patron ->
                            (factionOf player).power // 4

                        Just (Under pid) ->
                            case findInstance game pid of
                                Just parent ->
                                    (cardOf parent).transfer // 2

                                Nothing ->
                                    0

                        Nothing ->
                            0


defenseSupport : Game -> Group -> Int
defenseSupport game target =
    case target.ownerId of
        Nothing ->
            0

        Just oid ->
            let
                owner =
                    getPlayer game oid
            in
            case target.parent of
                Just Patron ->
                    (factionOf owner).power // 4

                Just (Under pid) ->
                    case findInstance game pid of
                        Just parent ->
                            (cardOf parent).transfer // 2

                        Nothing ->
                            0

                Nothing ->
                    0


specialMod : Game -> Player -> Attacker -> Group -> Kind -> Int
specialMod game player attacker target kind =
    let
        tCard =
            cardOf target

        hunterHere =
            case attacker of
                GroupAtk iid ->
                    case findInstance game iid of
                        Just a ->
                            (cardOf a).special == Just "founder-hunter"

                        Nothing ->
                            False

                PatronAtk ->
                    False

        controlsHunter =
            List.any (\g -> (cardOf g).special == Just "founder-hunter") player.grove

        owner =
            case target.ownerId of
                Just oid ->
                    Just (getPlayer game oid)

                Nothing ->
                    Nothing
    in
    (if player.factionId == "helios" && (tCard.type_ == "lab" || tCard.type_ == "chipmaker") then
        1

     else
        0
    )
        + (if player.factionId == "open-wave" && List.member "Open" (alignmentsOf target) then
            2

           else
            0
          )
        + (if player.factionId == "pause" && kind /= Control && List.member "Frontier" tCard.traits then
            3

           else
            0
          )
        + (if player.factionId == "directorate" && (List.member "Founder" tCard.traits || tCard.type_ == "lab") then
            2

           else
            0
          )
        + (if player.factionId == "crown" && (tCard.type_ == "power" || tCard.type_ == "datacenter" || tCard.type_ == "chipmaker") then
            2

           else
            0
          )
        + (if hunterHere && List.member "Founder" tCard.traits then
            2

           else
            0
          )
        + (if controlsHunter && List.member "Founder" tCard.traits && not hunterHere then
            2

           else
            0
          )
        + (if game.flags.exportControls > 0 && tCard.type_ == "chipmaker" then
            case owner of
                Just o ->
                    if o.factionId /= "long-plan" then
                        2

                    else
                        0

                Nothing ->
                    2

           else
            0
          )
        + (if game.flags.safetyLetter > 0 && List.member "Accelerationist" (alignmentsOf target) then
            if kind == Control then
                -1

            else
                1

           else
            0
          )


canAttack : Game -> Player -> Attacker -> Group -> Kind -> Result String ()
canAttack game player attacker target kind =
    if game.gameOver then
        Err "Game over"

    else if target.privilegedTurns > 0 then
        Err ((cardOf target).name ++ " is privileged and cannot be attacked yet.")

    else if player.attackedThisTurn then
        Err "You have already launched an attack this turn."

    else
        let
            leader =
                case attacker of
                    PatronAtk ->
                        if player.exhausted then
                            Err "Your patron already led an attack this turn."

                        else
                            Ok ()

                    GroupAtk iid ->
                        case findInstance game iid of
                            Nothing ->
                                Err "That group is not yours."

                            Just atk ->
                                if atk.ownerId /= Just player.id then
                                    Err "That group is not yours."

                                else if atk.exhausted then
                                    Err ((cardOf atk).name ++ " already acted this turn.")

                                else if atk.disabledTurns > 0 then
                                    Err ((cardOf atk).name ++ " is occupied and cannot lead an attack.")

                                else if game.flags.visaFreeze > 0 && (cardOf atk).type_ == "talent" then
                                    Err "Visa freeze: Talent cannot lead attacks this round."

                                else
                                    Ok ()
        in
        case leader of
            Err e ->
                Err e

            Ok () ->
                case kind of
                    Control ->
                        if player.controlledThisTurn then
                            Err "You may only take control of one group per turn."

                        else if target.ownerId == Just player.id then
                            Err "You already control that group."

                        else if List.isEmpty (freeParents player) then
                            Err "No free control sockets. Drop or reorganize first."

                        else
                            Ok ()

                    Neutralize ->
                        if target.ownerId == Nothing then
                            Err "Uncontrolled groups are already neutral."

                        else if target.ownerId == Just player.id then
                            Err "You would not neutralize your own group — drop it instead."

                        else
                            Ok ()

                    Destroy ->
                        Ok ()


suggestSpend : Game -> Player -> Attacker -> Group -> Kind -> { spend : Int, breakdown : Breakdown }
suggestSpend game player attacker target kind =
    let
        cap =
            min (totalMoney player) maxAttackSpend

        step s best =
            if s > cap then
                best

            else
                let
                    b =
                        attackStrength game player.id attacker target.instanceId kind s 0
                in
                if b.prob >= aimProbability then
                    { spend = s, breakdown = b }

                else if b.prob > best.breakdown.prob then
                    step (s + 1) { spend = s, breakdown = b }

                else
                    step (s + 1) best

        zero =
            attackStrength game player.id attacker target.instanceId kind 0 0
    in
    step 0 { spend = 0, breakdown = zero }


{-| The spend the attack dialog opens on. Prefer the most you can afford, up to
4, unless the cheaper aim-spend buys more odds per dollar.
-}
bestValueSpend : Game -> Player -> Attacker -> Group -> Kind -> Int
bestValueSpend game player attacker target kind =
    let
        cap =
            min (totalMoney player) maxAttackSpend

        aimed =
            (suggestSpend game player attacker target kind).spend

        perDollar s =
            if s <= 0 then
                0

            else
                (attackStrength game player.id attacker target.instanceId kind s 0).prob / toFloat s
    in
    if cap <= 0 then
        0

    else if aimed < cap && perDollar aimed > perDollar cap then
        aimed

    else
        cap


legalActions : Game -> List Action
legalActions game =
    if game.gameOver then
        []

    else
        let
            player =
                current game

            attackers =
                PatronAtk :: List.map (\g -> GroupAtk g.instanceId) player.grove

            targets =
                game.uncontrolled ++ List.concatMap .grove (List.filter (\p -> p.id /= player.id) game.players)

            kinds t =
                if t.ownerId == Nothing then
                    [ Control, Destroy ]

                else
                    [ Control, Neutralize, Destroy ]

            attacks =
                List.concatMap
                    (\atk ->
                        List.concatMap
                            (\t ->
                                List.filterMap
                                    (\kind ->
                                        case canAttack game player atk t kind of
                                            Ok () ->
                                                let
                                                    sug =
                                                        suggestSpend game player atk t kind
                                                in
                                                Just (Attack kind atk t.instanceId sug.spend Nothing)

                                            Err _ ->
                                                Nothing
                                    )
                                    (kinds t)
                            )
                            targets
                    )
                    attackers

            leak =
                if player.factionId == "open-wave" && List.any (\g -> (cardOf g).type_ == "lab") player.grove && totalMoney player >= leakCost && not game.flags.weightsLeak then
                    [ Leak ]

                else
                    []

            leans =
                if totalMoney player < 2 then
                    []

                else
                    List.filterMap
                        (\t ->
                            if t.ownerId == Just player.id || pressureOn player t.instanceId >= 6 then
                                Nothing

                            else
                                Just (Lean t.instanceId)
                        )
                        targets

            drops =
                List.map (\g -> Drop g.instanceId) player.grove

            reorgs =
                let
                    parents =
                        Patron :: List.map (\g -> Under g.instanceId) player.grove
                in
                List.concatMap
                    (\g ->
                        List.filterMap
                            (\parent ->
                                let
                                    desc =
                                        subtree player g.instanceId

                                    underSelf =
                                        case parent of
                                            Under iid ->
                                                List.any (\d -> d.instanceId == iid) desc

                                            Patron ->
                                                False
                                in
                                if underSelf || parent == Under g.instanceId || g.parent == Just parent || not (hasFree player parent) then
                                    Nothing

                                else
                                    Just (Reorg g.instanceId parent)
                            )
                            parents
                    )
                    player.grove
        in
        Pass :: attacks ++ leans ++ leak ++ drops ++ reorgs


applyAction : Game -> Action -> ActResult
applyAction game action =
    if game.gameOver then
        { ok = False, error = Just "Game over", game = game }

    else
        case action of
            Pass ->
                done (passAction game)

            Attack kind attacker targetId spend parent ->
                resolveAttack game kind attacker targetId spend parent

            Drop targetId ->
                dropGroup game targetId

            Reorg targetId parent ->
                reorgGroup game targetId parent

            Leak ->
                leakWeights game

            Lean targetId ->
                leanOn game targetId


resolveAttack : Game -> Kind -> Attacker -> String -> Int -> Maybe Parent -> ActResult
resolveAttack game kind attacker targetId spend0 parent =
    let
        player =
            current game
    in
    case findInstance game targetId of
        Nothing ->
            { ok = False, error = Just "Target gone.", game = game }

        Just target ->
            case canAttack game player attacker target kind of
                Err reason ->
                    { ok = False, error = Just reason, game = game }

                Ok () ->
                    let
                        spend =
                            clamp 0 maxAttackSpend spend0
                    in
                    if spend > totalMoney player then
                        { ok = False, error = Just "Not enough money.", game = game }

                    else
                        let
                            defender =
                                case target.ownerId of
                                    Just oid ->
                                        Just (getPlayer game oid)

                                    Nothing ->
                                        Nothing

                            defSpend =
                                case defender of
                                    Just d ->
                                        if d.isAI || d.agent == Reasonable then
                                            min (aiDefense game d target player kind spend) (totalMoney d)

                                        else
                                            0

                                    Nothing ->
                                        0

                            breakdown =
                                attackStrength game player.id attacker target.instanceId kind spend defSpend

                            gamePaid =
                                game
                                    |> updatePlayer player.id (takeMoney spend >> clearPressure target.instanceId)
                                    |> (\g ->
                                            case defender of
                                                Just d ->
                                                    if defSpend > 0 then
                                                        updatePlayer d.id (takeMoney defSpend) g

                                                    else
                                                        g

                                                Nothing ->
                                                    g
                                       )
                                    |> markAttacker player.id attacker

                            ( dice, gameRolled ) =
                                roll2d6 gamePaid

                            success =
                                dice.sum /= 12 && dice.sum <= breakdown.strength

                            atkName =
                                case attacker of
                                    PatronAtk ->
                                        (factionOf player).name

                                    GroupAtk iid ->
                                        case findInstance gameRolled iid of
                                            Just g ->
                                                (cardOf g).name

                                            Nothing ->
                                                "A group"

                            odds =
                                round (breakdown.prob * 100)

                            gameLogged =
                                { gameRolled
                                    | lastRoll =
                                        Just
                                            { a = dice.a
                                            , b = dice.b
                                            , sum = dice.sum
                                            , strength = breakdown.strength
                                            , success = success
                                            , kind = kindKey kind
                                            , targetName = (cardOf target).name
                                            , attackerName = atkName
                                            }
                                }
                                    |> say
                                        (player.name
                                            ++ " "
                                            ++ agrees player.name (kindThird kind)
                                            ++ " "
                                            ++ (cardOf target).name
                                            ++ " (need "
                                            ++ String.fromInt (max 2 breakdown.strength)
                                            ++ " or less, "
                                            ++ String.fromInt odds
                                            ++ "%). Rolls "
                                            ++ String.fromInt dice.a
                                            ++ "+"
                                            ++ String.fromInt dice.b
                                            ++ "="
                                            ++ String.fromInt dice.sum
                                            ++ ". "
                                            ++ (if success then
                                                    "SUCCESS."

                                                else
                                                    "Fails."
                                               )
                                        )
                                        (Just "attack")
                                        (Just success)

                            gameEffect =
                                if not success then
                                    updatePlayer player.id (\p -> { p | statsFailed = p.statsFailed + 1 }) gameLogged

                                else if kind == Control then
                                    steal gameLogged player.id target.instanceId parent
                                        |> updatePlayer player.id (\p -> { p | statsControlled = p.statsControlled + 1, controlledThisTurn = True })

                                else if kind == Neutralize then
                                    case target.ownerId of
                                        Just oid ->
                                            let
                                                gameCap =
                                                    if List.member "Frontier" (cardOf target).traits then
                                                        updatePlayer player.id
                                                            (\p ->
                                                                if List.member target.cardId p.capturedFrontier then
                                                                    p

                                                                else
                                                                    { p | capturedFrontier = p.capturedFrontier ++ [ target.cardId ] }
                                                            )
                                                            gameLogged

                                                    else
                                                        gameLogged
                                            in
                                            neutralize gameCap oid target.instanceId False

                                        Nothing ->
                                            gameLogged

                                else
                                    let
                                        gameCap =
                                            case target.ownerId of
                                                Just _ ->
                                                    if List.member "Frontier" (cardOf target).traits then
                                                        updatePlayer player.id
                                                            (\p ->
                                                                if List.member target.cardId p.capturedFrontier then
                                                                    p

                                                                else
                                                                    { p | capturedFrontier = p.capturedFrontier ++ [ target.cardId ] }
                                                            )
                                                            gameLogged

                                                    else
                                                        gameLogged

                                                Nothing ->
                                                    gameLogged
                                    in
                                    case target.ownerId of
                                        Nothing ->
                                            destroyWild gameCap target.instanceId

                                        Just oid ->
                                            destroyOwned gameCap oid target.instanceId
                        in
                        done (checkVictory (spendAction gameEffect))


aiDefense : Game -> Player -> Group -> Player -> Kind -> Int -> Int
aiDefense game defender target attacker kind atkSpend =
    let
        money =
            totalMoney defender
    in
    if money <= 0 then
        0

    else
        let
            cap =
                min money (max 2 ((money * 2) // 5))

            loop s =
                if s > cap then
                    cap

                else
                    let
                        b =
                            attackStrength game attacker.id PatronAtk target.instanceId kind atkSpend s
                    in
                    if b.prob <= 0.4 then
                        s

                    else
                        loop (s + 1)
        in
        loop 0


steal : Game -> Int -> String -> Maybe Parent -> Game
steal game playerId targetId preferred =
    case ( findInstance game targetId, Just (getPlayer game playerId) ) of
        ( Just target, Just player ) ->
            let
                parents =
                    freeParents player

                card =
                    cardOf target
            in
            if List.isEmpty parents then
                addLog game (player.name ++ " " ++ agrees player.name "has" ++ " no socket for " ++ card.name ++ "; the attack succeeds but the group slips away.") Nothing Nothing

            else
                let
                    parent =
                        chooseParent player card parents preferred

                    moving =
                        case target.ownerId of
                            Just oid ->
                                subtree (getPlayer game oid) target.instanceId

                            Nothing ->
                                [ target ]

                    prepared =
                        List.map
                            (\g ->
                                if g.instanceId == target.instanceId then
                                    { g | ownerId = Just playerId, parent = Just parent, privilegedTurns = 4 }

                                else
                                    { g | ownerId = Just playerId, privilegedTurns = 4 }
                            )
                            moving

                    game1 =
                        case target.ownerId of
                            Just oid ->
                                let
                                    ids =
                                        List.map .instanceId moving

                                    old =
                                        getPlayer game oid

                                    n =
                                        List.length moving

                                    extra =
                                        if n > 1 then
                                            " (and " ++ String.fromInt (n - 1) ++ " subordinate group" ++ (if n > 2 then "s" else "") ++ ")"

                                        else
                                            ""
                                in
                                updatePlayer oid (\p -> { p | grove = List.filter (\g -> not (List.member g.instanceId ids)) p.grove }) game
                                    |> say (player.name ++ " " ++ agrees player.name "wrests" ++ " " ++ card.name ++ " from " ++ old.name ++ extra ++ ".") Nothing Nothing

                            Nothing ->
                                { game | uncontrolled = List.filter (\g -> g.instanceId /= target.instanceId) game.uncontrolled }
                                    |> say (player.name ++ " " ++ agrees player.name "takes" ++ " control of " ++ card.name ++ ".") Nothing Nothing
                in
                game1
                    |> updatePlayer playerId
                        (\p ->
                            let
                                ids =
                                    List.map .instanceId p.grove
                            in
                            { p | grove = p.grove ++ List.filter (\g -> not (List.member g.instanceId ids)) prepared }
                        )
                    |> refill

        _ ->
            game


chooseParent : Player -> Card -> List Parent -> Maybe Parent -> Parent
chooseParent player card parents preferred =
    case preferred of
        Just p ->
            if List.member p parents then
                p

            else
                fallbackParent player card parents

        Nothing ->
            fallbackParent player card parents


fallbackParent : Player -> Card -> List Parent -> Parent
fallbackParent player card parents =
    let
        anchors =
            List.filter (\p -> p /= Patron) parents
    in
    if card.sockets <= 1 && not (List.isEmpty anchors) && socketsUsed player Patron >= 1 then
        case anchors of
            p :: _ ->
                p

            [] ->
                Patron

    else if List.member Patron parents then
        Patron

    else
        case parents of
            p :: _ ->
                p

            [] ->
                Patron


neutralize : Game -> Int -> String -> Bool -> Game
neutralize game ownerId targetId free =
    let
        owner =
            getPlayer game ownerId
    in
    case List.filter (\g -> g.instanceId == targetId) owner.grove of
        target :: _ ->
            let
                moving =
                    subtree owner targetId

                kids =
                    List.filter (\g -> g.instanceId /= targetId) moving

                ids =
                    List.map .instanceId moving

                cleared =
                    List.map (\g -> { g | ownerId = Nothing, parent = Nothing, privilegedTurns = 0, exhausted = False }) moving

                game1 =
                    updatePlayer ownerId (\p -> { p | grove = List.filter (\g -> not (List.member g.instanceId ids)) p.grove }) game

                game2 =
                    { game1 | uncontrolled = game1.uncontrolled ++ cleared }

                phrase =
                    case List.length kids of
                        0 ->
                            ""

                        1 ->
                            " along with 1 subordinate"

                        n ->
                            " along with " ++ String.fromInt n ++ " subordinates"
            in
            (if free then
                game2

             else
                addLog game2 ((cardOf target).name ++ " is neutralized and returns to the open field" ++ phrase ++ ".") Nothing Nothing
            )
                |> refill

        [] ->
            game


destroyOwned : Game -> Int -> String -> Game
destroyOwned game ownerId targetId =
    let
        owner =
            getPlayer game ownerId
    in
    case List.filter (\g -> g.instanceId == targetId) owner.grove of
        target :: _ ->
            let
                moving =
                    subtree owner targetId

                kids =
                    List.filter (\g -> g.instanceId /= targetId) moving

                ids =
                    List.map .instanceId moving

                cleared =
                    List.map (\g -> { g | ownerId = Nothing, parent = Nothing, privilegedTurns = 0, exhausted = False }) kids

                drift =
                    case List.length kids of
                        0 ->
                            ""

                        1 ->
                            " 1 subordinate drifts back to the open field."

                        n ->
                            " " ++ String.fromInt n ++ " subordinates drift back to the open field."
            in
            updatePlayer ownerId (\p -> { p | grove = List.filter (\g -> not (List.member g.instanceId ids)) p.grove }) game
                |> (\g -> { g | uncontrolled = g.uncontrolled ++ cleared })
                |> say ((cardOf target).name ++ " is destroyed and leaves the window." ++ drift) Nothing Nothing
                |> refill

        [] ->
            game


destroyWild : Game -> String -> Game
destroyWild game targetId =
    case List.filter (\g -> g.instanceId == targetId) game.uncontrolled of
        target :: _ ->
            { game | uncontrolled = List.filter (\g -> g.instanceId /= targetId) game.uncontrolled }
                |> say ((cardOf target).name ++ " is destroyed in the open field and leaves the window.") Nothing Nothing
                |> refill

        [] ->
            game


dropGroup : Game -> String -> ActResult
dropGroup game targetId =
    let
        player =
            current game
    in
    case findInstance game targetId of
        Just target ->
            if target.ownerId /= Just player.id then
                { ok = False, error = Just "Not your group.", game = game }

            else
                done
                    (game
                        |> say (player.name ++ " " ++ agrees player.name "releases" ++ " " ++ (cardOf target).name ++ ".") Nothing Nothing
                        |> (\g -> neutralize g player.id targetId False)
                        |> spendAction
                        |> checkVictory
                    )

        Nothing ->
            { ok = False, error = Just "Not your group.", game = game }


reorgGroup : Game -> String -> Parent -> ActResult
reorgGroup game targetId parent =
    let
        player =
            current game
    in
    case findInstance game targetId of
        Nothing ->
            { ok = False, error = Just "Not your group.", game = game }

        Just target ->
            if target.ownerId /= Just player.id then
                { ok = False, error = Just "Not your group.", game = game }

            else if parent == Under target.instanceId then
                { ok = False, error = Just "Cannot attach a group to itself.", game = game }

            else if
                case parent of
                    Under iid ->
                        List.any (\g -> g.instanceId == iid) (subtree player target.instanceId)

                    Patron ->
                        False
            then
                { ok = False, error = Just "Cannot attach a group under its own subordinate.", game = game }

            else if not (hasFree player parent) then
                { ok = False, error = Just "That parent has no free socket.", game = game }

            else
                let
                    label =
                        case parent of
                            Patron ->
                                "the patron"

                            Under iid ->
                                case findInstance game iid of
                                    Just g ->
                                        (cardOf g).name

                                    Nothing ->
                                        "a group"
                in
                done
                    (mapInstance targetId (\g -> { g | parent = Just parent }) game
                        |> say (player.name ++ " " ++ agrees player.name "reorganizes" ++ " " ++ (cardOf target).name ++ " under " ++ label ++ ".") Nothing Nothing
                        |> spendAction
                        |> checkVictory
                    )


leakWeights : Game -> ActResult
leakWeights game =
    let
        player =
            current game
    in
    if player.factionId /= "open-wave" then
        { ok = False, error = Just "Only The Open Wave can attempt a directed leak.", game = game }

    else
        case List.filter (\g -> (cardOf g).type_ == "lab") player.grove of
            [] ->
                { ok = False, error = Just "You need to control a Lab to leak its weights.", game = game }

            lab :: _ ->
                if totalMoney player < leakCost then
                    { ok = False, error = Just "Need 4 money to stage a leak.", game = game }

                else
                    let
                        game1 =
                            updatePlayer player.id (takeMoney leakCost) game

                        ( dice, game2 ) =
                            roll2d6 game1

                        success =
                            dice.sum /= 12 && dice.sum <= leakNeed

                        fresh =
                            getPlayer game2 player.id

                        game3 =
                            { game2
                                | lastRoll =
                                    Just
                                        { a = dice.a
                                        , b = dice.b
                                        , sum = dice.sum
                                        , strength = leakNeed
                                        , success = success
                                        , kind = "leak"
                                        , targetName = "the weights"
                                        , attackerName = fresh.name
                                        }
                            }
                                |> say
                                    (fresh.name
                                        ++ " "
                                        ++ agrees fresh.name "tries"
                                        ++ " to leak weights (need "
                                        ++ String.fromInt leakNeed
                                        ++ " or less). Rolls "
                                        ++ String.fromInt dice.a
                                        ++ "+"
                                        ++ String.fromInt dice.b
                                        ++ "="
                                        ++ String.fromInt dice.sum
                                        ++ ". "
                                        ++ (if success then
                                                "The files are out."

                                            else
                                                "The repo stays dark."
                                           )
                                    )
                                    Nothing
                                    Nothing

                        game4 =
                            if success then
                                markLeaked { game3 | flags = setLeak True game3.flags }
                                    |> mapInstance lab.instanceId addOpen
                                    |> updatePlayer player.id (\p -> { p | leaked = True })

                            else
                                game3
                    in
                    done (checkVictory (spendAction game4))


leanOn : Game -> String -> ActResult
leanOn game targetId =
    let
        player =
            current game
    in
    case findInstance game targetId of
        Nothing ->
            { ok = False, error = Just "That group is gone.", game = game }

        Just target ->
            if target.ownerId == Just player.id then
                { ok = False, error = Just "Lean on someone else's group.", game = game }

            else if totalMoney player < 2 then
                { ok = False, error = Just "Leaning costs 2 money.", game = game }

            else if pressureOn player targetId >= 6 then
                { ok = False, error = Just "You are already leaning as hard as you can.", game = game }

            else
                let
                    fresh =
                        getPlayer game player.id
                in
                done
                    (game
                        |> updatePlayer player.id (takeMoney 2 >> addPressure targetId)
                        |> say (fresh.name ++ " " ++ agrees fresh.name "leans" ++ " on " ++ (cardOf target).name ++ " (+2 on the next attack, up to +6).") Nothing Nothing
                        |> spendAction
                        |> checkVictory
                    )


passAction : Game -> Game
passAction game =
    let
        player =
            current game
    in
    endTurn (addLog { game | actionsLeft = 0 } (player.name ++ " " ++ agrees player.name "passes" ++ ".") Nothing Nothing)



-- Helpers


done : Game -> ActResult
done game =
    { ok = True, error = Nothing, game = game }


roll2d6 : Game -> ( Dice, Game )
roll2d6 game =
    case game.scriptedRolls of
        ( a, b ) :: rest ->
            ( { a = a, b = b, sum = a + b }, { game | scriptedRolls = rest } )

        [] ->
            let
                ( u1, r1 ) =
                    Rng.nextFloat game.rng

                ( u2, r2 ) =
                    Rng.nextFloat r1
            in
            ( { a = 1 + floor (u1 * 6), b = 1 + floor (u2 * 6), sum = 1 + floor (u1 * 6) + 1 + floor (u2 * 6) }, { game | rng = r2 } )


takeMoney : Int -> Player -> Player
takeMoney amount player =
    let
        fromPatron =
            min player.money amount

        need0 =
            amount - fromPatron

        ordered =
            List.sortBy (\g -> -g.money) player.grove

        deductions =
            Tuple.second
                (List.foldl
                    (\g ( need, acc ) ->
                        if need <= 0 then
                            ( need, acc )

                        else
                            let
                                take =
                                    min g.money need
                            in
                            ( need - take, ( g.instanceId, take ) :: acc )
                    )
                    ( need0, [] )
                    ordered
                )
    in
    { player
        | money = player.money - fromPatron
        , grove =
            List.map
                (\g ->
                    case List.filter (\( iid, _ ) -> iid == g.instanceId) deductions of
                        ( _, n ) :: _ ->
                            { g | money = g.money - n }

                        [] ->
                            g
                )
                player.grove
    }


markAttacker : Int -> Attacker -> Game -> Game
markAttacker pid attacker game =
    case attacker of
        PatronAtk ->
            updatePlayer pid (\p -> { p | exhausted = True, attackedThisTurn = True }) game

        GroupAtk iid ->
            updatePlayer pid (\p -> { p | attackedThisTurn = True }) game
                |> mapInstance iid (\g -> { g | exhausted = True })


refill : Game -> Game
refill game =
    if List.length game.uncontrolled >= 6 then
        game

    else
        case popEnd game.groupDeck of
            Nothing ->
                game

            Just ( g, deck ) ->
                refill { game | groupDeck = deck, uncontrolled = game.uncontrolled ++ [ g ] }


addLog : Game -> String -> Maybe String -> Maybe Bool -> Game
addLog game text kind success =
    { game | log = game.log ++ [ { when = yearLabel game, text = text, kind = kind, success = success } ] }


say : String -> Maybe String -> Maybe Bool -> Game -> Game
say text kind success game =
    addLog game text kind success


updatePlayer : Int -> (Player -> Player) -> Game -> Game
updatePlayer pid fn game =
    { game | players = List.map (\p -> if p.id == pid then fn p else p) game.players }


replacePlayer : Game -> Player -> Game
replacePlayer game p =
    updatePlayer p.id (\_ -> p) game


mapInstance : String -> (Group -> Group) -> Game -> Game
mapInstance iid fn game =
    { game
        | uncontrolled = List.map (touch iid fn) game.uncontrolled
        , players = List.map (\p -> { p | grove = List.map (touch iid fn) p.grove }) game.players
    }


touch : String -> (Group -> Group) -> Group -> Group
touch iid fn g =
    if g.instanceId == iid then
        fn g

    else
        g


allInstances : Game -> List Group
allInstances game =
    game.uncontrolled ++ List.concatMap .grove game.players


addOpen : Group -> Group
addOpen g =
    if List.member "Open" g.extraAlignments then
        g

    else
        { g | extraAlignments = g.extraAlignments ++ [ "Open" ] }


markLeaked : Game -> Game
markLeaked game =
    { game | players = List.map (\p -> if p.factionId == "open-wave" then { p | leaked = True } else p) game.players }


findFaction : Game -> String -> Maybe Player
findFaction game fid =
    List.filter (\p -> p.factionId == fid) game.players |> List.head


powerCount : Player -> Int
powerCount p =
    List.length (List.filter (\g -> (cardOf g).type_ == "power") p.grove)


kindThird : Kind -> String
kindThird kind =
    case kind of
        Control ->
            "controls"

        Neutralize ->
            "neutralizes"

        Destroy ->
            "destroys"


speak : Player -> String
speak p =
    if p.name == "You" && not p.isAI then
        "You"

    else
        (factionOf p).short


have : Player -> String
have p =
    if speak p == "You" then
        "have"

    else
        "has"


hold : Player -> String
hold p =
    if speak p == "You" then
        "hold"

    else
        "holds"


doNot : Player -> String
doNot p =
    if speak p == "You" then
        "do not"

    else
        "does not"


pressureOn : Player -> String -> Int
pressureOn player targetId =
    case List.filter (\( iid, _ ) -> iid == targetId) player.pressure of
        ( _, n ) :: _ ->
            n

        [] ->
            0


addPressure : String -> Player -> Player
addPressure targetId player =
    let
        next =
            min 6 (pressureOn player targetId + 2)

        rest =
            List.filter (\( iid, _ ) -> iid /= targetId) player.pressure
    in
    { player | pressure = ( targetId, next ) :: rest }


clearPressure : String -> Player -> Player
clearPressure targetId player =
    { player | pressure = List.filter (\( iid, _ ) -> iid /= targetId) player.pressure }


agrees : String -> String -> String
agrees name third =
    if name /= "You" then
        third

    else
        case third of
            "is" ->
                "are"

            "has" ->
                "have"

            "tries" ->
                "try"

            "collects" ->
                "collect"

            "controls" ->
                "control"

            "neutralizes" ->
                "neutralize"

            "destroys" ->
                "destroy"

            "passes" ->
                "pass"

            "releases" ->
                "release"

            "reorganizes" ->
                "reorganize"

            "wrests" ->
                "wrest"

            "takes" ->
                "take"

            "pockets" ->
                "pocket"

            "rides" ->
                "ride"

            "picks" ->
                "pick"

            "banks" ->
                "bank"

            "skims" ->
                "skim"

            "invokes" ->
                "invoke"

            "receives" ->
                "receive"

            "raises" ->
                "raise"

            "scores" ->
                "score"

            "leans" ->
                "lean"

            _ ->
                third


setExport : Int -> Flags -> Flags
setExport n f =
    { f | exportControls = n }


setGpu : Int -> Flags -> Flags
setGpu n f =
    { f | gpuShortage = n }


setVisa : Int -> Flags -> Flags
setVisa n f =
    { f | visaFreeze = n }


setSafety : Int -> Flags -> Flags
setSafety n f =
    { f | safetyLetter = n }


setLeak : Bool -> Flags -> Flags
setLeak b f =
    { f | weightsLeak = b }


dec : Int -> Int
dec n =
    if n > 0 then
        n - 1

    else
        0


popMany : Int -> List a -> ( List a, List a )
popMany n xs =
    let
        len =
            List.length xs

        keep =
            max 0 (len - n)
    in
    ( List.take keep xs, List.reverse (List.drop keep xs) )


popEnd : List a -> Maybe ( a, List a )
popEnd xs =
    case List.reverse xs of
        [] ->
            Nothing

        y :: ys ->
            Just ( y, List.reverse ys )


pick : List a -> Int -> ( Maybe a, Int )
pick xs rng =
    case xs of
        [] ->
            ( Nothing, rng )

        _ ->
            let
                ( u, r1 ) =
                    Rng.nextFloat rng

                i =
                    floor (u * toFloat (List.length xs))
            in
            ( List.drop i xs |> List.head, r1 )


pickGame : Game -> List a -> ( Maybe a, Game )
pickGame game xs =
    let
        ( hit, rng ) =
            pick xs game.rng
    in
    ( hit, { game | rng = rng } )


shuffle : List a -> Int -> ( List a, Int )
shuffle items rng0 =
    let
        n =
            List.length items

        step i arr rng =
            if i <= 0 then
                ( arr, rng )

            else
                let
                    ( u, rngNext ) =
                        Rng.nextFloat rng

                    j =
                        floor (u * toFloat (i + 1))
                in
                case ( Array.get i arr, Array.get j arr ) of
                    ( Just a, Just b ) ->
                        step (i - 1) (arr |> Array.set i b |> Array.set j a) rngNext

                    _ ->
                        step (i - 1) arr rngNext

        ( out, rngDone ) =
            step (n - 1) (Array.fromList items) rng0
    in
    ( Array.toList out, rngDone )


unique : List a -> List a
unique xs =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc

            else
                acc ++ [ x ]
        )
        []
        xs


firstJust : (a -> Maybe b) -> List a -> Maybe b
firstJust f xs =
    case xs of
        [] ->
            Nothing

        y :: rest ->
            case f y of
                Just v ->
                    Just v

                Nothing ->
                    firstJust f rest
