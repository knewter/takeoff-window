port module Main exposing (main)

import Ai
import Browser
import Browser.Events
import Engine exposing (Action(..), Attacker(..), Kind(..), Parent(..))
import Html exposing (Html, pre, text)
import Html.Attributes as A
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import Rng
import Simulate
import Task
import Time
import View exposing (Model, Msg(..), Screen(..), Tab(..))


port outgoing : Encode.Value -> Cmd msg


port incoming : (Decode.Value -> msg) -> Sub msg


type Msg
    = Ui View.Msg
    | Tick
    | Resize Int Int
    | FromJs Decode.Value
    | GotTime Time.Posix
    | FinishRoll
    | UnlockVictory


type alias App =
    { ui : Model
    , reply : Maybe Int
    , now : Int
    , headless : Maybe String
    }


main : Program Decode.Value App Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Decode.Value -> ( App, Cmd Msg )
init flags =
    let
        f =
            decodeFlags flags

        ui =
            View.initUi f.width f.height (Maybe.withDefault "" f.seed)
                |> (\m ->
                        { m
                            | numAI = clamp 2 3 f.numAI
                            , speed = if f.speed > 0 then f.speed else 420
                            , selectedFaction = f.faction
                            , hintSeen = f.hintSeen
                        }
                   )

        app =
            { ui = ui, reply = Nothing, now = f.now, headless = Nothing }
    in
    if f.headless then
        let
            report =
                Simulate.run { games = f.n, humanGames = 0, seed = Rng.seedFrom (Maybe.withDefault (String.fromInt f.now) f.seed) }
        in
        ( { app | headless = Just (Encode.encode 2 (Simulate.encode report)) }, Cmd.none )

    else if f.autoplay then
        bootGame app (Maybe.withDefault (String.fromInt f.now) f.seed) True True Nothing

    else if f.autostart then
        bootGame app (Maybe.withDefault (String.fromInt f.now) f.seed) (not f.reveal) False Nothing

    else if f.screen == Just "faction" then
        emit { app | ui = { ui | screen = View.FactionPick } } Cmd.none

    else
        emit app Cmd.none


view : App -> Html Msg
view app =
    case app.headless of
        Just json ->
            pre [ A.id "results", A.style "color" "#e8e4d8", A.style "background" "#090b0f", A.style "padding" "24px", A.style "white-space" "pre-wrap" ] [ text json ]

        Nothing ->
            Html.map Ui (View.view app.ui)


update : Msg -> App -> ( App, Cmd Msg )
update msg app =
    case msg of
        Ui uiMsg ->
            updateUi uiMsg app

        Tick ->
            stepAI app

        Resize w h ->
            emit (mapUi (\ui -> { ui | width = w, height = h }) app) Cmd.none

        GotTime posix ->
            let
                n =
                    Time.posixToMillis posix |> modBy 1000000000 |> String.fromInt
            in
            emit (mapUi (\ui -> { ui | seedInput = n }) { app | now = Time.posixToMillis posix }) Cmd.none

        FinishRoll ->
            case app.ui.modal of
                View.Rolling form ->
                    case app.ui.game of
                        Just game ->
                            if game.gameOver then
                                presentVictory app

                            else
                                emit (mapUi (\ui -> { ui | modal = View.RollResult form }) app) Cmd.none

                        Nothing ->
                            ( app, Cmd.none )

                _ ->
                    ( app, Cmd.none )

        UnlockVictory ->
            emit (mapUi (\ui -> { ui | actionsReady = True }) app) Cmd.none

        FromJs value ->
            fromJs value app


updateUi : View.Msg -> App -> ( App, Cmd Msg )
updateUi msg app =
    let
        ui =
            app.ui
    in
    case msg of
        NoOp ->
            ( app, Cmd.none )

        GotoTitle ->
            let
                fresh =
                    View.initUi ui.width ui.height ui.seedInput
            in
            emit { app | ui = { fresh | hintSeen = ui.hintSeen, numAI = ui.numAI } } Cmd.none

        GotoFaction ->
            if ui.modal == View.VictoryModal && not ui.actionsReady then
                ( app, Cmd.none )

            else
                emit { app | ui = { ui | screen = View.FactionPick, modal = View.NoModal, game = Nothing, driveAll = False, busy = False, actionsReady = True } } Cmd.none

        PickFaction id ->
            emit { app | ui = { ui | selectedFaction = Just id } } Cmd.none

        SetAI raw ->
            emit { app | ui = { ui | numAI = clamp 2 3 (Maybe.withDefault 3 (String.toInt raw)) } } Cmd.none

        SeedInput raw ->
            emit { app | ui = { ui | seedInput = raw } } Cmd.none

        RandomSeed ->
            ( app, Task.perform GotTime Time.now )

        Start ->
            case ui.selectedFaction of
                Nothing ->
                    ( app, Cmd.none )

                Just _ ->
                    bootGame app (if ui.seedInput == "" then String.fromInt app.now else ui.seedInput) False False Nothing

        OpenWindow ->
            emit { app | ui = maybeEvent { ui | screen = View.Play } } Cmd.none

        SetTab tab ->
            case tab of
                View.TabLog ->
                    emit { app | ui = { ui | modal = View.Drawer "log" } } Cmd.none

                View.TabRules ->
                    emit { app | ui = { ui | modal = View.Drawer "rules" } } Cmd.none

                _ ->
                    emit { app | ui = { ui | tab = tab, modal = if ui.modal == View.Help then View.NoModal else ui.modal } } Cmd.none

        OpenGoal ->
            emit { app | ui = { ui | modal = View.Drawer "goal" } } Cmd.none

        Select id ->
            emit { app | ui = { ui | selectedId = Just id } } Cmd.none

        DismissHint ->
            emit (mapUi (\base -> { base | hintSeen = True }) app) (outgoing (Encode.object [ ( "tag", Encode.string "dismissHint" ) ]))

        ViewPlayer id ->
            emit { app | ui = { ui | viewingPlayer = id, selectedId = Just "patron", tab = View.TabTree } } Cmd.none

        OpenAttack id kind ->
            case ui.game of
                Just game ->
                    case Engine.findInstance game id of
                        Just target ->
                            let
                                you =
                                    Engine.getPlayer game 0

                                usable =
                                    (PatronAtk :: List.map (\g -> GroupAtk g.instanceId) you.grove)
                                        |> List.filter
                                            (\atk ->
                                                case Engine.canAttack game you atk target kind of
                                                    Ok _ ->
                                                        True

                                                    Err _ ->
                                                        False
                                            )

                                attacker =
                                    if List.member PatronAtk usable then
                                        PatronAtk

                                    else
                                        List.head usable |> Maybe.withDefault PatronAtk

                                sug =
                                    Engine.bestValueSpend game you attacker target kind

                                parents =
                                    Engine.freeParents you

                                parent =
                                    if List.member Patron parents then
                                        Patron

                                    else
                                        List.head parents |> Maybe.withDefault Patron
                            in
                            emit { app | ui = { ui | modal = View.AttackModal { targetId = id, kind = kind, attacker = attacker, spend = sug, parent = parent } } } Cmd.none

                        Nothing ->
                            ( app, Cmd.none )

                Nothing ->
                    ( app, Cmd.none )

        SetAttacker attacker ->
            case ( ui.modal, ui.game ) of
                ( View.AttackModal form, Just game ) ->
                    case Engine.findInstance game form.targetId of
                        Just target ->
                            let
                                sug =
                                    Engine.bestValueSpend game (Engine.getPlayer game 0) attacker target form.kind
                            in
                            emit { app | ui = { ui | modal = View.AttackModal { form | attacker = attacker, spend = sug } } } Cmd.none

                        Nothing ->
                            ( app, Cmd.none )

                _ ->
                    ( app, Cmd.none )

        SetParent parent ->
            case ui.modal of
                View.AttackModal form ->
                    emit { app | ui = { ui | modal = View.AttackModal { form | parent = parent } } } Cmd.none

                View.ReorgModal id _ ->
                    emit { app | ui = { ui | modal = View.ReorgModal id parent } } Cmd.none

                _ ->
                    ( app, Cmd.none )

        SetSpend raw ->
            case ui.modal of
                View.AttackModal form ->
                    let
                        n =
                            Maybe.withDefault 0 (String.toInt raw)
                    in
                    emit { app | ui = { ui | modal = View.AttackModal { form | spend = clamp 0 Engine.maxAttackSpend n } } } Cmd.none

                _ ->
                    ( app, Cmd.none )

        Launch ->
            case ( ui.modal, ui.game ) of
                ( View.AttackModal form, Just game ) ->
                    let
                        result =
                            Engine.applyAction game (Attack form.kind form.attacker form.targetId form.spend (Just form.parent))

                        nextGame =
                            if result.ok then
                                result.game

                            else
                                game

                        ui1 =
                            { ui | game = Just nextGame, modal = View.Rolling form }
                    in
                    if nextGame.gameOver then
                        presentVictory { app | ui = ui1 }

                    else
                        emit { app | ui = ui1 } (Process.sleep 650 |> Task.perform (\_ -> FinishRoll))

                _ ->
                    ( app, Cmd.none )

        CloseModal ->
            case ui.modal of
                View.Rolling _ ->
                    ( app, Cmd.none )

                View.RollResult _ ->
                    afterAction { app | ui = { ui | modal = View.NoModal } }

                View.VictoryModal ->
                    ( app, Cmd.none )

                _ ->
                    emit { app | ui = { ui | modal = View.NoModal } } Cmd.none

        ReviewBoard ->
            if ui.modal == View.VictoryModal && ui.actionsReady then
                emit { app | ui = { ui | modal = View.NoModal } } Cmd.none

            else
                ( app, Cmd.none )

        Backdrop ->
            case ui.modal of
                View.Rolling _ ->
                    ( app, Cmd.none )

                View.VictoryModal ->
                    ( app, Cmd.none )

                View.NoModal ->
                    ( app, Cmd.none )

                _ ->
                    updateUi CloseModal app

        PassTurn ->
            apply (Pass) app

        DoLeak ->
            apply Leak app

        DoLean id ->
            apply (Lean id) app

        AskDrop id ->
            emit { app | ui = { ui | modal = View.Confirm "Release this group?" "It returns to the open field, along with any subordinates. This costs an action." (DoDrop id) } } Cmd.none

        DoDrop id ->
            apply (Drop id) { app | ui = { ui | modal = View.NoModal } }

        OpenReorg id ->
            emit { app | ui = { ui | modal = View.ReorgModal id Patron } } Cmd.none

        DoReorg ->
            case ( ui.modal, ui.game ) of
                ( View.ReorgModal id parent, Just _ ) ->
                    apply (Reorg id parent) { app | ui = { ui | modal = View.NoModal } }

                _ ->
                    ( app, Cmd.none )

        ShowVictory ->
            emit { app | ui = { ui | modal = View.VictoryModal } } Cmd.none

        Rematch ->
            if ui.modal == View.VictoryModal && not ui.actionsReady then
                ( app, Cmd.none )

            else
                case ui.game of
                    Just game ->
                        let
                            faction =
                                (Engine.getPlayer game 0).factionId

                            ui1 =
                                { ui | selectedFaction = Just faction, seedInput = String.fromInt (modBy 1000000000 (app.now + game.turnNumber * 17 + 1)) }
                        in
                        bootGame { app | ui = ui1 } ui1.seedInput False False Nothing

                    Nothing ->
                        ( app, Cmd.none )

        ShowHelp ->
            if View.phone ui && ui.screen == View.Play then
                emit { app | ui = { ui | tab = View.TabRules, modal = View.NoModal } } Cmd.none

            else
                emit { app | ui = { ui | modal = View.Help } } Cmd.none

        ShowEvent ->
            emit { app | ui = { ui | modal = View.EventModal } } Cmd.none

        SpeedUp ->
            emit { app | ui = { ui | speed = 40 } } Cmd.none


apply : Action -> App -> ( App, Cmd Msg )
apply action app =
    case app.ui.game of
        Nothing ->
            ( app, Cmd.none )

        Just game ->
            let
                result =
                    Engine.applyAction game action

                next =
                    if result.ok then
                        result.game

                    else
                        game
            in
            afterAction (mapUi (\ui -> { ui | game = Just next, modal = View.NoModal }) app)


afterAction : App -> ( App, Cmd Msg )
afterAction app =
    case app.ui.game of
        Just game ->
            if game.gameOver then
                presentVictory app

            else
                schedule app

        Nothing ->
            ( app, Cmd.none )


stepAI : App -> ( App, Cmd Msg )
stepAI app =
    case app.ui.game of
        Nothing ->
            ( app, Cmd.none )

        Just game ->
            if game.gameOver || not (shouldDrive app.ui game) then
                schedule app

            else
                let
                    ( action, game1 ) =
                        Ai.chooseAI game

                    result =
                        Engine.applyAction game1 action

                    next =
                        if result.ok then
                            result.game

                        else
                            (Engine.applyAction result.game Pass).game
                in
                schedule (mapUi (\ui -> { ui | game = Just next }) app)


schedule : App -> ( App, Cmd Msg )
schedule app =
    case app.ui.game of
        Nothing ->
            ( app, Cmd.none )

        Just game ->
            if game.gameOver then
                presentVictory app

            else if shouldDrive app.ui game then
                emit (mapUi (\ui -> { ui | busy = True }) app) (Process.sleep (toFloat (max 0 app.ui.speed)) |> Task.perform (\_ -> Tick))

            else
                emit (mapUi (\ui -> maybeEvent { ui | busy = False }) app) Cmd.none


shouldDrive : Model -> Engine.Game -> Bool
shouldDrive ui game =
    ui.driveAll || (Engine.current game).isAI


maybeEvent : Model -> Model
maybeEvent ui =
    case ui.game of
        Just game ->
            case game.lastEvent of
                Just ev ->
                    let
                        key =
                            ev.id ++ "@" ++ ev.when
                    in
                    if ui.seenEvent /= Just key && not (Engine.current game).isAI && ui.modal == View.NoModal && ui.screen == View.Play && not ui.driveAll then
                        { ui | seenEvent = Just key, modal = View.EventModal }

                    else
                        ui

                Nothing ->
                    ui

        Nothing ->
            ui


bootGame : App -> String -> Bool -> Bool -> Maybe Int -> ( App, Cmd Msg )
bootGame app seedText skipReveal drive reply =
    let
        ui0 =
            app.ui

        game =
            Engine.createGame
                { seed = Rng.seedFrom seedText
                , playerFaction = ui0.selectedFaction
                , numAI = ui0.numAI
                , playerName = "You"
                }

        ui =
            { ui0
                | game = Just game
                , screen = if skipReveal then View.Play else View.Reveal
                , driveAll = drive
                , busy = drive
                , modal = View.NoModal
                , viewingPlayer = 0
                , selectedId = Nothing
                , tab = View.TabTree
                , seenEvent = Nothing
                , actionsReady = True
            }

        started =
            { app | ui = if skipReveal then maybeEvent ui else ui, reply = orKeep reply app.reply }
    in
    if drive then
        schedule started

    else
        case started.reply of
            Just id ->
                emit { started | reply = Nothing } (outgoing (replyValue id (stateValue (Just game))))

            Nothing ->
                emit started Cmd.none


orKeep : Maybe Int -> Maybe Int -> Maybe Int
orKeep new old =
    case new of
        Just _ ->
            new

        Nothing ->
            old


presentVictory : App -> ( App, Cmd Msg )
presentVictory app =
    let
        shown =
            mapUi
                (\ui ->
                    { ui
                        | busy = False
                        , driveAll = False
                        , modal = View.VictoryModal
                        , screen = View.Play
                        , actionsReady = False
                    }
                )
                app

        ( app2, cmd ) =
            finish shown
    in
    ( app2, Cmd.batch [ cmd, Process.sleep 800 |> Task.perform (\_ -> UnlockVictory) ] )


finish : App -> ( App, Cmd Msg )
finish app =
    case app.reply of
        Just id ->
            emit { app | reply = Nothing } (outgoing (replyValue id (stateValue app.ui.game)))

        Nothing ->
            emit app Cmd.none


fromJs : Decode.Value -> App -> ( App, Cmd Msg )
fromJs value app =
    case Decode.decodeValue commandDecoder value of
        Err _ ->
            ( app, Cmd.none )

        Ok cmd ->
            case cmd.tag of
                "start" ->
                    let
                        base =
                            app.ui

                        ui =
                            { base
                                | selectedFaction = cmd.faction |> orElse base.selectedFaction
                                , numAI = Maybe.withDefault base.numAI cmd.numAI
                                , seedInput = Maybe.withDefault base.seedInput cmd.seed
                            }
                    in
                    bootGame { app | ui = ui } (Maybe.withDefault (String.fromInt app.now) (orElse cmd.seed (blankToNothing ui.seedInput))) cmd.skipReveal cmd.autoplay (Just cmd.id)

                "autoplay" ->
                    case app.ui.game of
                        Just _ ->
                            schedule { app | reply = Just cmd.id, ui = let base = app.ui in { base | driveAll = True, speed = 0, screen = View.Play } }

                        Nothing ->
                            bootGame app (Maybe.withDefault (String.fromInt app.now) (blankToNothing app.ui.seedInput)) True True (Just cmd.id)

                "playToEnd" ->
                    case app.ui.game of
                        Just game ->
                            let
                                done =
                                    Ai.run 900 game
                            in
                            finish
                                { app
                                    | reply = Just cmd.id
                                    , ui =
                                        let
                                            base =
                                                app.ui
                                        in
                                        { base
                                            | game = Just done
                                            , screen = View.Play
                                            , modal = if done.gameOver then View.VictoryModal else View.NoModal
                                            , busy = False
                                            , driveAll = False
                                            , actionsReady = True
                                        }
                                }

                        Nothing ->
                            replyNow app cmd.id Encode.null

                "playTurns" ->
                    case app.ui.game of
                        Just game ->
                            let
                                done =
                                    runTurns (Maybe.withDefault 8 cmd.n) game
                            in
                            emit
                                (mapUi
                                    (\ui ->
                                        { ui
                                            | game = Just done
                                            , screen = View.Play
                                            , busy = False
                                            , driveAll = False
                                            , actionsReady = True
                                            , modal = if done.gameOver then View.VictoryModal else View.NoModal
                                        }
                                    )
                                    app
                                )
                                (outgoing (replyValue cmd.id (stateValue (Just done))))

                        Nothing ->
                            replyNow app cmd.id Encode.null

                "runHeadless" ->
                    let
                        report =
                            Simulate.run
                                { games = Maybe.withDefault 24 cmd.n
                                , humanGames = Maybe.withDefault 0 cmd.human
                                , seed = Maybe.withDefault 2026 cmd.seedInt
                                }
                    in
                    replyNow app cmd.id (Simulate.encode report)

                _ ->
                    replyNow app cmd.id (stateValue app.ui.game)


runTurns : Int -> Engine.Game -> Engine.Game
runTurns n game =
    if n <= 0 || game.gameOver then
        game

    else
        let
            ( action, game1 ) =
                Ai.chooseAI game

            result =
                Engine.applyAction game1 action

            next =
                if result.ok then
                    result.game

                else
                    (Engine.applyAction result.game Pass).game
        in
        runTurns (n - 1) next


replyNow : App -> Int -> Encode.Value -> ( App, Cmd Msg )
replyNow app id payload =
    emit app (outgoing (replyValue id payload))


emit : App -> Cmd Msg -> ( App, Cmd Msg )
emit app cmd =
    ( app, Cmd.batch [ cmd, outgoing (Encode.object [ ( "tag", Encode.string "state" ), ( "state", stateValue app.ui.game ) ]) ] )


replyValue : Int -> Encode.Value -> Encode.Value
replyValue id payload =
    Encode.object
        [ ( "tag", Encode.string "reply" )
        , ( "id", Encode.int id )
        , ( "payload", payload )
        ]


stateValue : Maybe Engine.Game -> Encode.Value
stateValue game =
    case game of
        Nothing ->
            Encode.null

        Just g ->
            Encode.object
                [ ( "seed", Encode.int g.seed )
                , ( "year", Encode.string (Engine.yearLabel g) )
                , ( "round", Encode.int g.round )
                , ( "turn", Encode.int g.turnNumber )
                , ( "actionsLeft", Encode.int g.actionsLeft )
                , ( "currentPlayer", Encode.int g.currentPlayer )
                , ( "gameOver", Encode.bool g.gameOver )
                , ( "winner", maybeInt g.winner )
                , ( "winReason", maybeString g.winReason )
                , ( "log", Encode.list encodeLog (List.take 40 (List.reverse g.log) |> List.reverse) )
                , ( "players", Encode.list (encodePlayer g) g.players )
                ]


encodeLog : Engine.LogEntry -> Encode.Value
encodeLog entry =
    Encode.object
        [ ( "when", Encode.string entry.when )
        , ( "text", Encode.string entry.text )
        ]


encodePlayer : Engine.Game -> Engine.Player -> Encode.Value
encodePlayer _ p =
    Encode.object
        [ ( "id", Encode.int p.id )
        , ( "name", Encode.string p.name )
        , ( "factionId", Encode.string p.factionId )
        , ( "isAI", Encode.bool p.isAI )
        , ( "money", Encode.int (Engine.totalMoney p) )
        , ( "compute", Encode.int p.compute )
        , ( "groups", Encode.int (Engine.groupCount p) )
        ]


maybeInt : Maybe Int -> Encode.Value
maybeInt m =
    case m of
        Just n ->
            Encode.int n

        Nothing ->
            Encode.null


maybeString : Maybe String -> Encode.Value
maybeString m =
    case m of
        Just s ->
            Encode.string s

        Nothing ->
            Encode.null


subscriptions : App -> Sub Msg
subscriptions _ =
    Sub.batch
        [ incoming FromJs
        , Browser.Events.onResize Resize
        , Browser.Events.onKeyDown (Decode.field "key" Decode.string |> Decode.map (\k -> if k == "Escape" then Ui CloseModal else Ui NoOp))
        ]


type alias Command =
    { id : Int
    , tag : String
    , seed : Maybe String
    , faction : Maybe String
    , numAI : Maybe Int
    , skipReveal : Bool
    , autoplay : Bool
    , n : Maybe Int
    , human : Maybe Int
    , seedInt : Maybe Int
    }


commandDecoder : Decode.Decoder Command
commandDecoder =
    Decode.map8
        (\id tag seed faction numAI skip auto n ->
            { id = id
            , tag = tag
            , seed = seed
            , faction = faction
            , numAI = numAI
            , skipReveal = skip
            , autoplay = auto
            , n = n
            , human = Nothing
            , seedInt = Nothing
            }
        )
        (Decode.field "id" Decode.int)
        (Decode.field "tag" Decode.string)
        (Decode.maybe (Decode.field "seed" Decode.string))
        (Decode.maybe (Decode.field "faction" Decode.string))
        (Decode.maybe (Decode.field "numAI" Decode.int))
        (Decode.oneOf [ Decode.field "skipReveal" Decode.bool, Decode.succeed False ])
        (Decode.oneOf [ Decode.field "autoplay" Decode.bool, Decode.succeed False ])
        (Decode.maybe (Decode.field "n" Decode.int))
        |> Decode.andThen
            (\cmd ->
                Decode.map2
                    (\human seedInt -> { cmd | human = human, seedInt = seedInt })
                    (Decode.maybe (Decode.field "human" Decode.int))
                    (Decode.maybe (Decode.field "seedInt" Decode.int))
            )


type alias BootFlags =
    { seed : Maybe String
    , autoplay : Bool
    , faction : Maybe String
    , numAI : Int
    , screen : Maybe String
    , autostart : Bool
    , reveal : Bool
    , width : Int
    , height : Int
    , headless : Bool
    , n : Int
    , speed : Int
    , now : Int
    , hintSeen : Bool
    }


decodeFlags : Decode.Value -> BootFlags
decodeFlags value =
    Decode.decodeValue
        (Decode.map8
            (\seed autoplay faction numAI screen autostart reveal width ->
                { seed = seed
                , autoplay = autoplay
                , faction = faction
                , numAI = numAI
                , screen = screen
                , autostart = autostart
                , reveal = reveal
                , width = width
                , height = 800
                , headless = False
                , n = 20
                , speed = 420
                , now = 2026
                , hintSeen = False
                }
            )
            (Decode.maybe (Decode.field "seed" Decode.string))
            (Decode.oneOf [ Decode.field "autoplay" Decode.bool, Decode.succeed False ])
            (Decode.maybe (Decode.field "faction" Decode.string))
            (Decode.oneOf [ Decode.field "numAI" Decode.int, Decode.succeed 3 ])
            (Decode.maybe (Decode.field "screen" Decode.string))
            (Decode.oneOf [ Decode.field "autostart" Decode.bool, Decode.succeed False ])
            (Decode.oneOf [ Decode.field "reveal" Decode.bool, Decode.succeed True ])
            (Decode.oneOf [ Decode.field "width" Decode.int, Decode.succeed 1280 ])
        )
        value
        |> Result.map
            (\partial ->
                Decode.decodeValue
                    (Decode.map6
                        (\height headless n speed now hintSeen ->
                            { partial | height = height, headless = headless, n = n, speed = speed, now = now, hintSeen = hintSeen }
                        )
                        (Decode.oneOf [ Decode.field "height" Decode.int, Decode.succeed 800 ])
                        (Decode.oneOf [ Decode.field "headless" Decode.bool, Decode.succeed False ])
                        (Decode.oneOf [ Decode.field "n" Decode.int, Decode.succeed 20 ])
                        (Decode.oneOf [ Decode.field "speed" Decode.int, Decode.succeed 420 ])
                        (Decode.oneOf [ Decode.field "now" Decode.int, Decode.succeed 2026 ])
                        (Decode.oneOf [ Decode.field "hintSeen" Decode.bool, Decode.succeed False ])
                    )
                    value
                    |> Result.withDefault partial
            )
        |> Result.withDefault
            { seed = Nothing
            , autoplay = False
            , faction = Nothing
            , numAI = 3
            , screen = Nothing
            , autostart = False
            , reveal = True
            , width = 1280
            , height = 800
            , headless = False
            , n = 20
            , speed = 420
            , now = 2026
            , hintSeen = False
            }


blankToNothing : String -> Maybe String
blankToNothing s =
    if String.trim s == "" then
        Nothing

    else
        Just s


orElse : Maybe a -> Maybe a -> Maybe a
orElse a b =
    case a of
        Just _ ->
            a

        Nothing ->
            b


mapUi : (Model -> Model) -> App -> App
mapUi fn app =
    { app | ui = fn app.ui }
