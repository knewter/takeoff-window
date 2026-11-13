module View exposing (Modal(..), Model, Msg(..), Screen(..), Tab(..), initUi, landscape, phone, view)

import Data exposing (Alignment, Card, Faction, TypeMeta, alignments, cardById, disclaimer, factionById, factions, subtitle, title, typeMeta)
import Engine exposing (Action(..), Attacker(..), Game, Group, Kind(..), Parent(..), Player)
import Html exposing (Html, button, div, h1, h2, h3, input, label, li, nav, p, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes as A exposing (attribute, class, classList, disabled, style, type_, value)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Json.Decode as Decode
import Svg exposing (Svg, svg)
import Svg.Attributes as SA
import Svg.Events


type Screen
    = Title
    | FactionPick
    | Reveal
    | Play


type Tab
    = TabTree
    | TabRivals
    | TabMarket
    | TabLog
    | TabRules


type Modal
    = NoModal
    | Help
    | EventModal
    | AttackModal AttackForm
    | Rolling AttackForm
    | RollResult AttackForm
    | VictoryModal
    | ReorgModal String Parent
    | Confirm String String Msg
    | CardSheet String


type alias AttackForm =
    { targetId : String
    , kind : Kind
    , attacker : Attacker
    , spend : Int
    , parent : Parent
    }


type alias Model =
    { screen : Screen
    , game : Maybe Game
    , selectedFaction : Maybe String
    , numAI : Int
    , seedInput : String
    , selectedId : Maybe String
    , viewingPlayer : Int
    , modal : Modal
    , tab : Tab
    , width : Int
    , height : Int
    , seenEvent : Maybe String
    , busy : Bool
    , speed : Int
    , driveAll : Bool
    , actionsReady : Bool
    }


type Msg
    = NoOp
    | GotoTitle
    | GotoFaction
    | PickFaction String
    | SetAI String
    | SeedInput String
    | RandomSeed
    | Start
    | OpenWindow
    | SetTab Tab
    | Select String
    | ViewPlayer Int
    | OpenAttack String Kind
    | SetAttacker Attacker
    | SetParent Parent
    | SetSpend String
    | Launch
    | CloseModal
    | ReviewBoard
    | PassTurn
    | DoLeak
    | DoLean String
    | AskDrop String
    | DoDrop String
    | OpenReorg String
    | DoReorg
    | ShowVictory
    | Rematch
    | ShowHelp
    | ShowEvent
    | SpeedUp
    | Backdrop


initUi : Int -> Int -> String -> Model
initUi width height seedInput =
    { screen = Title
    , game = Nothing
    , selectedFaction = Nothing
    , numAI = 3
    , seedInput = seedInput
    , selectedId = Nothing
    , viewingPlayer = 0
    , modal = NoModal
    , tab = TabTree
    , width = width
    , height = height
    , seenEvent = Nothing
    , busy = False
    , speed = 420
    , driveAll = False
    , actionsReady = True
    }


phone : Model -> Bool
phone model =
    model.width <= 820 || (model.height <= 520 && model.width <= 960)


landscape : Model -> Bool
landscape model =
    phone model && model.height <= 520


view : Model -> Html Msg
view model =
    div [ class "play-root", style "height" "100%", style "min-height" "0", style "display" "flex", style "flex-direction" "column" ]
        [ case model.screen of
            Title ->
                viewTitle

            FactionPick ->
                viewFactions model

            Reveal ->
                viewReveal model

            Play ->
                viewPlay model
        , viewModal model
        ]


viewTitle : Html Msg
viewTitle =
    div [ class "screen-center", style "position" "relative" ]
        [ div [ style "position" "relative", style "z-index" "1", style "display" "flex", style "flex-direction" "column", style "align-items" "center" ]
            [ div [ class "years" ] [ text "2026  —  2031" ]
            , h1 [ class "wordmark" ] [ text "Takeoff", Html.br [] [], Html.em [] [ text "Window" ] ]
            , p [ class "subtitle" ] [ text subtitle ]
            , Html.hr [ class "gold-rule" ] []
            , p [ class "lede" ] [ text "The last five years before the frontier closes. You take a chair. Two or three rivals take the others. Each of you is a hidden patron with a secret goal, a treasury, and a hunger for the groups that actually run the world: fabs, labs, grids, funds, headlines. Attacks are rolled. Money talks. The window does not stay open." ]
            , div [ class "btn-row" ]
                [ button [ class "btn btn-primary", onClick GotoFaction ] [ text "New Game" ]
                , button [ class "btn btn-ghost", onClick ShowHelp ] [ text "How to Play" ]
                ]
            , p [ class "disclaimer" ] [ text (disclaimer ++ " This is an original work and is not affiliated with, endorsed by, or derived from any trademarked board game.") ]
            ]
        ]


viewFactions : Model -> Html Msg
viewFactions model =
    div [ class "screen-center" ]
        [ div [ class "years" ] [ text "CHOOSE YOUR PATRON" ]
        , h2 [ class "choose" ] [ text "One chair is yours. The goal stays secret." ]
        , div [ class "faction-grid" ] (List.map (factionCard model.selectedFaction) factions)
        , div [ class "faction-footer" ]
            [ div [ class "setup-bar" ]
                [ div [ class "ai-count" ]
                    [ text "Rivals "
                    , label [] [ input [ type_ "radio", A.name "ai", value "2", A.checked (model.numAI == 2), onInput SetAI ] [], text " 2 AI" ]
                    , label [] [ input [ type_ "radio", A.name "ai", value "3", A.checked (model.numAI == 3), onInput SetAI ] [], text " 3 AI" ]
                    ]
                , label [ class "field" ]
                    [ text "Seed"
                    , div [ class "seed-row" ]
                        [ input [ type_ "text", A.id "seed-input", value model.seedInput, A.placeholder "random", onInput SeedInput ] []
                        , button [ class "btn btn-sm btn-ghost", onClick RandomSeed ] [ text "Random" ]
                        ]
                    ]
                ]
            , div [ class "btn-row" ]
                [ button [ class "btn btn-ghost", onClick GotoTitle ] [ text "Back" ]
                , button [ class "btn btn-primary", disabled (model.selectedFaction == Nothing), onClick Start ] [ text "Take your seat" ]
                ]
            , p [ class "setup-note" ] [ text "A seed makes the deal, the dice, and the headlines reproducible." ]
            ]
        ]


factionCard : Maybe String -> Faction -> Html Msg
factionCard selected f =
    button
        [ classList [ ( "faction-card", True ), ( "selected", selected == Just f.id ) ]
        , style "--fc" f.color
        , onClick (PickFaction f.id)
        ]
        [ div [ class "stripe" ] []
        , h3 [] [ text f.name ]
        , div [ class "motto" ] [ text f.motto ]
        , div [ class "stats" ] [ text ("Power " ++ String.fromInt f.power ++ " · Income " ++ String.fromInt f.income ++ " · Sockets " ++ String.fromInt f.sockets) ]
        , div [ class "flavor" ] [ text f.flavor ]
        , div [ class "label" ] [ text "Secret goal" ]
        , div [ class "goal" ] [ text f.goalText ]
        , div [ class "label" ] [ text "Ability" ]
        , div [ class "ability" ] [ text f.ability ]
        ]


viewReveal : Model -> Html Msg
viewReveal model =
    case model.game of
        Nothing ->
            viewTitle

        Just game ->
            let
                you =
                    Engine.getPlayer game 0

                f =
                    factionOf you
            in
            div [ class "screen-center" ]
                [ div [ class "reveal-card", style "--fc" f.color ]
                    [ div [ class "stripe", style "background" f.color ] []
                    , div [ class "k" ] [ text ("Classified briefing · " ++ String.fromInt game.seed) ]
                    , h1 [ style "color" f.color ] [ text f.name ]
                    , div [ class "motto" ] [ text f.motto ]
                    , p [ class "flavor" ] [ text f.flavor ]
                    , div [ class "reveal-stats" ]
                        [ meter "Power" (String.fromInt f.power)
                        , meter "Income" (String.fromInt f.income)
                        , meter "Sockets" (String.fromInt f.sockets)
                        ]
                    , div [ class "goal-box" ]
                        [ Html.strong [] [ text "Secret goal" ]
                        , text f.goalText
                        , div [ class "goal-detail" ] [ text f.goalHint ]
                        ]
                    , p [ style "font-size" "13px", style "color" "var(--text-dim)" ] [ text f.ability ]
                    , p [ style "font-size" "13px", style "color" "var(--text-dim)" ]
                        [ text ("Your rivals have taken their chairs. Their goals stay hidden. Anyone can also win the public way: control " ++ String.fromInt Engine.publicGoal ++ " settled groups.") ]
                    , button [ class "btn btn-primary", onClick OpenWindow ] [ text "The window is open" ]
                    ]
                ]


viewPlay : Model -> Html Msg
viewPlay model =
    case model.game of
        Nothing ->
            viewTitle

        Just game ->
            if phone model then
                viewPhone model game

            else
                viewDesktop model game


viewDesktop : Model -> Game -> Html Msg
viewDesktop model game =
    let
        you =
            Engine.getPlayer game 0

        viewer =
            Engine.getPlayer game model.viewingPlayer
    in
    div [ style "display" "flex", style "flex-direction" "column", style "height" "100%", style "min-height" "0" ]
        [ topbar model game False
        , eventBanner game
        , div [ class "board" ]
            [ div [ class "col" ]
                [ youCard you game False
                , flagsHtml game
                , div [ class "panel-head" ] [ text "Turn log" ]
                , div [ class "log" ] (logEntries game)
                ]
            , div [ class "col col-center" ]
                [ div [ class "panel-head" ]
                    [ span [] [ text (if viewer.id == 0 then "Your structure" else (factionOf viewer).name ++ " · structure") ]
                    , span [] [ text (String.fromInt (Engine.groupCount viewer) ++ " groups") ]
                    ]
                , viewBanner viewer
                , treeSvg game viewer (viewer.id == 0 && not game.gameOver && not model.busy) False False model.selectedId
                , div [ class "panel-head market-head" ]
                    [ span [] [ text "The open field" ]
                    , span [] [ text (String.fromInt (List.length game.uncontrolled) ++ " in play · " ++ String.fromInt (List.length game.groupDeck) ++ " in the deck") ]
                    ]
                , div [ class "market" ] (marketCards game model.selectedId)
                ]
            , div [ class "col col-right" ]
                [ div [ class "panel-head" ]
                    [ span [] [ text "Rivals" ]
                    , span [] [ text "click a name to view their tree" ]
                    ]
                , div [ class "rivals" ] (rivalCards model game)
                ]
            , dock model game False
            ]
        ]


viewPhone : Model -> Game -> Html Msg
viewPhone model game =
    let
        you =
            Engine.getPlayer game 0

        viewer =
            Engine.getPlayer game model.viewingPlayer

        stage =
            case model.tab of
                TabTree ->
                    div []
                        [ youCard you game True
                        , viewBanner viewer
                        , div [ class "panel-head" ]
                            [ span [] [ text (if viewer.id == 0 then "Your structure" else (factionOf viewer).short ++ " · structure") ]
                            , span [] [ text (String.fromInt (Engine.groupCount viewer) ++ " groups") ]
                            ]
                        , div [ class "tree-mobile" ]
                            [ div [ class "tree-scroller" ] [ treeSvg game viewer (viewer.id == 0 && not game.gameOver && not model.busy) True (landscape model) model.selectedId ]
                            , treeList game viewer model.selectedId
                            ]
                        ]

                TabRivals ->
                    div []
                        [ div [ class "panel-head" ] [ span [] [ text "Rivals" ], span [] [ text "Tap a name to view their tree" ] ]
                        , div [ class "rivals" ] (rivalCards model game)
                        ]

                TabMarket ->
                    div []
                        [ div [ class "panel-head" ]
                            [ span [] [ text "The open field" ]
                            , span [] [ text (String.fromInt (List.length game.uncontrolled) ++ " in play · " ++ String.fromInt (List.length game.groupDeck) ++ " in the deck") ]
                            ]
                        , div [ class "market" ] (marketCards game model.selectedId)
                        ]

                TabLog ->
                    div []
                        [ flagsHtml game
                        , div [ class "panel-head" ] [ span [] [ text "Turn log" ], span [] [ text (Engine.yearLabel game) ] ]
                        , div [ class "log-phone" ] [ div [ class "log" ] (logEntries game) ]
                        ]

                TabRules ->
                    div [ class "rules-phone" ]
                        [ div [ class "help-body help-cols" ] [ rulesBody ]
                        , p [ class "setup-note" ] [ text ("Seed " ++ String.fromInt game.seed ++ ".") ]
                        , div [ class "btn-row" ] [ button [ class "btn btn-ghost", onClick GotoTitle ] [ text "New Game" ] ]
                        ]
    in
    div [ class "play-root", style "display" "flex", style "flex-direction" "column", style "height" "100%", style "min-height" "0" ]
        [ topbar model game True
        , eventBanner game
        , div [ class "phone-play" ]
            [ div [ class "phone-stage" ] [ stage ]
            , dock model game True
            ]
        , tabBar model.tab
        ]


topbar : Model -> Game -> Bool -> Html Msg
topbar model game isPhone =
    let
        cur =
            Engine.current game

        whose =
            if game.gameOver then
                span [ class "whose over" ] [ text "Window closed" ]

            else if cur.isAI || model.busy || model.driveAll then
                span [ class "whose rival" ] [ span [ class "busy-dot" ] [], text (cur.name ++ " is acting") ]

            else
                span [ class "whose" ] [ text ("Your turn · " ++ String.fromInt game.actionsLeft ++ (if game.actionsLeft == 1 then " action" else " actions")) ]
    in
    div [ class "topbar", classList [ ( "topbar-phone", isPhone ) ] ]
        [ h1 [ class "brand" ] [ text (if isPhone then "Takeoff" else "Takeoff Window") ]
        , div [ class "meta" ]
            [ span [] [ Html.strong [] [ text (Engine.yearLabel game) ] ]
            , span [] [ text (String.fromInt game.round ++ "/" ++ "20") ]
            , if isPhone then
                text ""

              else
                span [] [ text ("Seed " ++ String.fromInt game.seed) ]
            ]
        , if isPhone then
            text ""

          else
            timeline game
        , div [ class "spacer" ] []
        , whose
        , if isPhone then
            text ""

          else
            button [ class "btn btn-sm btn-ghost", onClick ShowHelp ] [ text "Rules" ]
        , if isPhone then
            text ""

          else
            button [ class "btn btn-sm btn-ghost", onClick GotoTitle ] [ text "New Game" ]
        ]


youCard : Player -> Game -> Bool -> Html Msg
youCard you game compact =
    let
        f =
            factionOf you

        goal =
            Engine.goalProgress game you

        pct =
            String.fromFloat (goal.ratio * 100) ++ "%"
    in
    div [ class "you-card", classList [ ( "you-card-compact", compact ) ], style "--fc" f.color ]
        [ div [ class "stripe" ] []
        , if compact then
            div [ class "you-compact-head" ]
                [ h2 [] [ text f.name ]
                , div [ class "you-inline-stats" ]
                    [ inlineMeter "money" "Money" (String.fromInt (Engine.totalMoney you))
                    , inlineMeter "compute" "Compute" (String.fromInt you.compute)
                    , inlineMeter "groups" "Groups" (String.fromInt (Engine.groupCount you))
                    , inlineMeter "actions" "Act" (if game.gameOver then "—" else String.fromInt game.actionsLeft)
                    ]
                ]

          else
            div []
                [ h2 [] [ text f.name ]
                , div [ class "motto" ] [ text f.motto ]
                , div [ class "meters" ]
                    [ meterBox "money" "Money" (String.fromInt (Engine.totalMoney you))
                    , meterBox "compute" "Compute" (String.fromInt you.compute)
                    , meterBox "groups" "Groups" (String.fromInt (Engine.groupCount you))
                    , meterBox "actions" "Act" (if game.gameOver then "—" else String.fromInt game.actionsLeft)
                    ]
                ]
        , div [ class "goal-box" ]
            [ if compact then
                text ""

              else
                Html.strong [] [ text "Secret goal" ]
            , if compact then
                text ""

              else
                text f.goalText
            , div [ class "goal-detail" ] [ text goal.secretText ]
            , div [ class "goal-detail race" ] [ text ("Compute race: " ++ computeRace game) ]
            , div [ class "goal-detail" ] [ text ("Public win: " ++ goal.publicText ++ ".") ]
            , div [ class "progress" ] [ Html.i [ style "width" pct, style "background" f.color ] [] ]
            ]
        , if compact then
            text ""

          else
            p [ class "ability" ] [ text f.ability ]
        ]


dock : Model -> Game -> Bool -> Html Msg
dock model game isPhone =
    let
        you =
            Engine.getPlayer game 0

        yourTurn =
            not game.gameOver && (Engine.current game).id == 0 && not model.busy && not model.driveAll

        ( name, meta ) =
            case model.selectedId of
                Just "patron" ->
                    let
                        viewer =
                            Engine.getPlayer game model.viewingPlayer

                        vf =
                            factionOf viewer
                    in
                    ( vf.name, "Patron · Power " ++ String.fromInt vf.power ++ " · Income " ++ String.fromInt vf.income )

                Just id ->
                    case Engine.findInstance game id of
                        Just g ->
                            let
                                card =
                                    Engine.cardOf g

                                owner =
                                    case g.ownerId of
                                        Just oid ->
                                            (factionOf (Engine.getPlayer game oid)).short

                                        Nothing ->
                                            "Open"
                            in
                            ( card.name, card.title ++ " · " ++ owner )

                        Nothing ->
                            ( "Gone", "" )

                Nothing ->
                    if yourTurn then
                        ( "Choose a group", "Click a card in the open field, on a tree, or in a rival's list." )

                    else if game.gameOver then
                        ( "The window has closed", "" )

                    else
                        ( "Waiting on a rival", "" )

        actions =
            if yourTurn then
                attackButtons game model.selectedId
                    ++ leakButton game you
                    ++ [ button [ class "btn btn-ghost", onClick PassTurn ] [ text "Pass" ] ]

            else if model.busy then
                [ button [ class "btn btn-sm btn-ghost", onClick SpeedUp ] [ text "Speed up" ] ]

            else
                []

        actions2 =
            if game.gameOver then
                actions ++ [ button [ class "btn btn-primary", onClick ShowVictory ] [ text "See the outcome" ] ]

            else
                actions
    in
    div [ class "dock", classList [ ( "dock-phone", isPhone ) ] ]
        [ if isPhone then
            div [ class "dock-money" ] [ text ("$ " ++ String.fromInt (Engine.totalMoney you)), span [] [ text "Money" ] ]

          else
            text ""
        , div [ class "dock-info" ]
            [ h3 [] [ text name ]
            , div [ class "meta" ] [ text meta ]
            ]
        , div [ class "dock-actions" ]
            (if List.isEmpty actions2 then
                [ span [ class "hint" ] [ text "Watch the log. The rivals are moving." ] ]

             else
                actions2
            )
        ]


attackButtons : Game -> Maybe String -> List (Html Msg)
attackButtons game selected =
    let
        you =
            Engine.getPlayer game 0

        yourTurn =
            not game.gameOver && (Engine.current game).id == 0
    in
    if not yourTurn then
        []

    else
        case selected of
            Just id ->
                if id == "patron" then
                    []

                else
                    case Engine.findInstance game id of
                        Just g ->
                            if g.ownerId == Just 0 then
                                [ button [ class "btn btn-sm", onClick (OpenReorg id) ] [ text "Reorganize" ]
                                , button [ class "btn btn-sm btn-danger", onClick (AskDrop id) ] [ text "Release" ]
                                ]

                            else if you.attackedThisTurn then
                                [ div [ class "attack-note" ] [ text "You have already launched an attack this turn." ]
                                , button [ class "btn btn-kind-control", disabled True, A.title "You have already launched an attack this turn." ] [ text "Control" ]
                                , button [ class "btn btn-kind-neutralize", disabled True, A.title "You have already launched an attack this turn." ] [ text "Neutralize" ]
                                , button [ class "btn btn-kind-destroy", disabled True, A.title "You have already launched an attack this turn." ] [ text "Destroy" ]
                                ]

                            else
                                let
                                    ctrl =
                                        Engine.canAttack game you PatronAtk g Control

                                    blocked =
                                        case ctrl of
                                            Err reason ->
                                                not (String.contains "already led" reason || String.contains "already acted" reason)

                                            Ok _ ->
                                                False

                                    lean =
                                        if Engine.totalMoney you >= 2 && Engine.pressureOn you id < 6 then
                                            [ button [ class "btn btn-sm", onClick (DoLean id) ] [ text ("Lean ($2, now +" ++ String.fromInt (Engine.pressureOn you id) ++ ")") ] ]

                                        else
                                            []
                                in
                                lean
                                    ++ (if g.ownerId == Nothing || g.ownerId /= Just 0 then
                                            [ button [ class "btn btn-kind-control", disabled blocked, A.title (reasonText ctrl), onClick (OpenAttack id Control) ] [ text "Control" ] ]

                                        else
                                            []
                                       )
                                    ++ (if g.ownerId == Nothing then
                                            []

                                        else
                                            [ button [ class "btn btn-kind-neutralize", onClick (OpenAttack id Neutralize) ] [ text "Neutralize" ] ]
                                       )
                                    ++ [ button [ class "btn btn-kind-destroy", onClick (OpenAttack id Destroy) ] [ text "Destroy" ] ]

                        Nothing ->
                            []

            Nothing ->
                []


leakButton : Game -> Player -> List (Html Msg)
leakButton game you =
    if you.factionId == "open-wave" && not game.flags.weightsLeak then
        let
            can =
                List.any (\g -> (Engine.cardOf g).type_ == "lab") you.grove && Engine.totalMoney you >= Engine.leakCost
        in
        [ button [ class "btn btn-sm", disabled (not can), onClick DoLeak ] [ text "Leak weights ($4)" ] ]

    else
        []


marketCards : Game -> Maybe String -> List (Html Msg)
marketCards game selected =
    if List.isEmpty game.uncontrolled then
        [ div [ class "empty-tree" ] [ text "The open field is empty. The deck is exhausted." ] ]

    else
        List.map (\g -> groupCard g (selected == Just g.instanceId) Nothing) game.uncontrolled


groupCard : Group -> Bool -> Maybe Player -> Html Msg
groupCard g selected owner =
    let
        card =
            Engine.cardOf g

        meta =
            typeOf card.type_
    in
    button
        [ classList [ ( "gcard", True ), ( "selected", selected ), ( "privileged", g.privilegedTurns > 0 ) ]
        , style "--tc" meta.color
        , onClick (Select g.instanceId)
        ]
        [ div [ class "art" ] [ cardArt card ]
        , if card.parody then
            span [ class "parody-stamp" ] [ text "PARODY" ]

          else
            text ""
        , div [ class "typebar" ] []
        , div [ class "body" ]
            [ div [ class "type" ] [ text (meta.label ++ (if card.parody then " · PARODY" else "") ++ (if g.exhausted then " · ACTED" else "") ++ (if g.disabledTurns > 0 then " · DISABLED" else "")) ]
            , div [ class "name" ] [ text card.name ]
            , div [ class "title" ] [ text card.title ]
            , div [ class "pills" ] (pills (Engine.alignmentsOf g))
            , div [ class "nums" ] (nums g owner)
            ]
        ]


rivalCards : Model -> Game -> List (Html Msg)
rivalCards model game =
    game.players
        |> List.filter (\p -> p.id /= 0)
        |> List.map
            (\p ->
                let
                    pf =
                        factionOf p
                in
                div
                    [ class "rival"
                    , classList [ ( "current", (Engine.current game).id == p.id ), ( "active", model.viewingPlayer == p.id ) ]
                    , style "--fc" pf.color
                    , onClick (ViewPlayer p.id)
                    ]
                    [ div [ class "rival-head" ]
                        [ h3 [ style "color" pf.color ] [ text pf.short ]
                        , span [ class "dots" ] [ text p.name ]
                        ]
                    , div [ class "rival-meters" ]
                        [ span [ class "m" ] [ text ("$ " ++ String.fromInt (Engine.totalMoney p)) ]
                        , span [ class "c" ] [ text ("C " ++ String.fromInt p.compute) ]
                        , span [] [ text ("G " ++ String.fromInt (Engine.groupCount p)) ]
                        ]
                    , div [ class "rival-chips" ]
                        (if List.isEmpty p.grove then
                            [ span [ style "color" "var(--faint)", style "font-size" "11px" ] [ text "No groups yet" ] ]

                         else
                            List.map
                                (\g ->
                                    let
                                        card =
                                            Engine.cardOf g
                                    in
                                    button
                                        [ class "rchip"
                                        , classList [ ( "selected", model.selectedId == Just g.instanceId ), ( "privileged", g.privilegedTurns > 0 ) ]
                                        , style "--tc" (typeOf card.type_).color
                                        , onClick (Select g.instanceId)
                                        ]
                                        [ text card.name ]
                                )
                                p.grove
                        )
                    ]
            )


eventBanner : Game -> Html Msg
eventBanner game =
    case game.lastEvent of
        Just ev ->
            div [ class "event-banner", onClick ShowEvent ]
                [ div [ class "k" ] [ text ("Headline · " ++ ev.when) ]
                , h3 [] [ text ev.name ]
                , p [] [ text ev.flavor ]
                ]

        Nothing ->
            text ""


flagsHtml : Game -> Html Msg
flagsHtml game =
    let
        bits =
            (if game.flags.weightsLeak then
                [ span [ class "flag good" ] [ text "Weights are out" ] ]

             else
                []
            )
                ++ (if game.flags.gpuShortage > 0 then
                        [ span [ class "flag warn" ] [ text ("GPU shortage (" ++ String.fromInt game.flags.gpuShortage ++ ")") ] ]

                    else
                        []
                   )
                ++ (if game.flags.exportControls > 0 then
                        [ span [ class "flag warn" ] [ text ("Export controls (" ++ String.fromInt game.flags.exportControls ++ ")") ] ]

                    else
                        []
                   )
                ++ (if game.flags.visaFreeze > 0 then
                        [ span [ class "flag amber" ] [ text "Visa freeze" ] ]

                    else
                        []
                   )
                ++ (if game.flags.safetyLetter > 0 then
                        [ span [ class "flag" ] [ text ("Safety letter (" ++ String.fromInt game.flags.safetyLetter ++ ")") ] ]

                    else
                        []
                   )
    in
    if List.isEmpty bits then
        text ""

    else
        div [ class "flags" ] bits


logEntries : Game -> List (Html Msg)
logEntries game =
    game.log
        |> List.reverse
        |> List.take 80
        |> List.reverse
        |> List.map
            (\e ->
                div
                    [ class "entry"
                    , classList
                        [ ( Maybe.withDefault "" e.kind, e.kind /= Nothing )
                        , ( "ok", e.kind == Just "attack" && e.success == Just True )
                        , ( "fail", e.kind == Just "attack" && e.success == Just False )
                        ]
                    ]
                    [ span [ class "when" ] [ text e.when ]
                    , text e.text
                    ]
            )


timeline : Game -> Html Msg
timeline game =
    let
        dots =
            List.concatMap
                (\r ->
                    let
                        year =
                            2026 + (r - 1) // 4

                        mark =
                            if modBy 4 (r - 1) == 0 then
                                [ span [ class "yr" ] [ text (String.fromInt year) ] ]

                            else
                                []
                    in
                    mark
                        ++ [ span
                                [ class "dot"
                                , classList [ ( "done", r < game.round ), ( "now", r == game.round ) ]
                                ]
                                []
                           ]
                )
                (List.range 1 20)
    in
    div [ class "timeline" ] dots


tabBar : Tab -> Html Msg
tabBar current =
    nav [ class "tabbar" ]
        (List.map
            (\( id, label ) ->
                button [ classList [ ( "on", current == id ) ], onClick (SetTab id) ] [ text label ]
            )
            [ ( TabTree, "Tree" ), ( TabRivals, "Rivals" ), ( TabMarket, "Open" ), ( TabLog, "Log" ), ( TabRules, "Rules" ) ]
        )


viewBanner : Player -> Html Msg
viewBanner viewer =
    if viewer.id == 0 then
        text ""

    else
        div [ class "viewing-banner" ]
            [ text ("Viewing " ++ (factionOf viewer).name ++ "'s structure")
            , button [ class "btn btn-sm btn-ghost", onClick (ViewPlayer 0) ] [ text "Back to yours" ]
            ]


treeList : Game -> Player -> Maybe String -> Html Msg
treeList game player selected =
    let
        f =
            factionOf player

        row g depth =
            let
                card =
                    Engine.cardOf g
            in
            button
                [ class "tree-row"
                , classList [ ( "selected", selected == Just g.instanceId ) ]
                , style "--tc" (typeOf card.type_).color
                , style "margin-left" (String.fromInt (min depth 4 * 16) ++ "px")
                , onClick (Select g.instanceId)
                ]
                [ span [ class "tr-stripe" ] []
                , span [ class "tr-body" ]
                    [ div [ class "tr-type" ] [ text (typeOf card.type_).label ]
                    , div [ class "tr-name" ] [ text card.name ]
                    , div [ class "tr-nums" ]
                        [ text ("P " ++ String.fromInt (displayPower g) ++ " · R " ++ String.fromInt (resistance g) ++ " · $ " ++ String.fromInt g.money) ]
                    ]
                ]

        walk parent depth =
            List.concatMap
                (\g -> row g depth :: walk (Under g.instanceId) (depth + 1))
                (List.filter (\g -> g.parent == Just parent) player.grove)
    in
    div [ class "tree-list" ]
        (button
            [ class "tree-row patron", classList [ ( "selected", selected == Just "patron" ) ], style "--tc" f.color, onClick (Select "patron") ]
            [ span [ class "tr-stripe" ] []
            , span [ class "tr-body" ]
                [ div [ class "tr-type" ] [ text "Patron" ]
                , div [ class "tr-name" ] [ text f.name ]
                ]
            ]
            :: walk Patron 1
        )



-- Tree


type Raw
    = Raw
        { key : String
        , selfW : Float
        , selfH : Float
        , w : Float
        , partsW : Float
        , kids : List Raw
        , empties : Int
        }


type Placed
    = Placed
        { key : String
        , x : Float
        , y : Float
        , selfW : Float
        , selfH : Float
        , kids : List Placed
        , slots : List ( Float, Float )
        }


treeSvg : Game -> Player -> Bool -> Bool -> Bool -> Maybe String -> Html Msg
treeSvg game player interactive compact land selected =
    let
        nw =
            if land then
                170

            else if compact then
                148

            else
                176

        nh =
            if land then
                76

            else if compact then
                84

            else
                104

        pw =
            if land then
                198

            else if compact then
                176

            else
                210

        ph =
            if land then
                66

            else if compact then
                78

            else
                90

        hg =
            if land then
                22

            else if compact then
                14

            else
                22

        vg =
            if land then
                26

            else if compact then
                36

            else
                64

        raw =
            measure player "patron" nw nh pw ph hg

        placed =
            place raw 0 0 hg vg

        ( maxX, maxY ) =
            case placed of
                Placed root ->
                    extent placed ( root.selfW, root.selfH )

        pad =
            if land then
                10

            else if compact then
                14

            else
                24

        width =
            maxX + pad * 2

        height =
            maxY + pad * 2

        color =
            (factionOf player).color
    in
    div [ class "tree-wrap" ]
        [ svg
            [ SA.width (String.fromFloat (max width 320))
            , SA.height (String.fromFloat (max height 160))
            , SA.viewBox ("-" ++ String.fromFloat pad ++ " -" ++ String.fromFloat pad ++ " " ++ String.fromFloat width ++ " " ++ String.fromFloat height)
            , A.attribute "role" "img"
            ]
            (Svg.defs []
                [ Svg.marker
                    [ SA.id ("arr-" ++ String.fromInt player.id), SA.markerWidth "9", SA.markerHeight "8", SA.refX "7", SA.refY "4", SA.orient "auto" ]
                    [ Svg.path [ SA.d "M0,0 L9,4 L0,8 L2.5,4 Z", SA.fill color ] [] ]
                ]
                :: drawEdges color player.id placed
                ++ drawNodes game player interactive compact selected placed
            )
        , if List.isEmpty player.grove then
            div [ class "empty-tree" ] [ text "No groups under this patron yet. Take one from the open field." ]

          else
            text ""
        ]


measure : Player -> String -> Float -> Float -> Float -> Float -> Float -> Raw
measure player key nw nh pw ph hg =
    let
        isPatron =
            key == "patron"

        parent =
            if isPatron then
                Patron

            else
                Under key

        kids =
            List.filter (\g -> g.parent == Just parent) player.grove

        maxSock =
            if isPatron then
                (factionOf player).sockets

            else
                case List.filter (\g -> g.instanceId == key) player.grove of
                    g :: _ ->
                        (Engine.cardOf g).sockets

                    [] ->
                        0

        empties =
            max 0 (maxSock - List.length kids)

        mkids =
            List.map (\g -> measure player g.instanceId nw nh pw ph hg) kids

        kidsW =
            List.sum (List.map (\(Raw k) -> k.w) mkids) + toFloat (max 0 (List.length mkids - 1)) * hg

        emptyW =
            if empties == 0 then
                0

            else
                toFloat empties * 44 + toFloat (max 0 (empties - 1)) * 8 + (if List.isEmpty mkids then 0 else hg)

        partsW =
            kidsW + emptyW

        selfW =
            if isPatron then
                pw

            else
                nw

        selfH =
            if isPatron then
                ph

            else
                nh
    in
    Raw { key = key, selfW = selfW, selfH = selfH, w = max selfW partsW, partsW = partsW, kids = mkids, empties = empties }


place : Raw -> Float -> Float -> Float -> Float -> Placed
place (Raw raw) x y hg vg =
    let
        nx =
            x + (raw.w - raw.selfW) / 2

        start =
            x + (raw.w - raw.partsW) / 2

        ( kids, cx ) =
            List.foldl
                (\(Raw k) ( acc, cursor ) ->
                    ( acc ++ [ place (Raw k) cursor (y + raw.selfH + vg) hg vg ], cursor + k.w + hg )
                )
                ( [], start )
                raw.kids

        slots =
            List.map (\i -> ( cx + toFloat i * 52 + 22, y + raw.selfH + vg + 18 )) (List.range 0 (raw.empties - 1))
    in
    Placed { key = raw.key, x = nx, y = y, selfW = raw.selfW, selfH = raw.selfH, kids = kids, slots = slots }


extent : Placed -> ( Float, Float ) -> ( Float, Float )
extent (Placed node) ( mx, my ) =
    let
        base =
            ( max mx (node.x + node.selfW), max my (node.y + node.selfH) )

        withSlots =
            List.foldl (\( sx, sy ) ( ax, ay ) -> ( max ax (sx + 16), max ay (sy + 16) )) base node.slots
    in
    List.foldl (\k acc -> extent k acc) withSlots node.kids


drawEdges : String -> Int -> Placed -> List (Svg Msg)
drawEdges color pid (Placed node) =
    let
        x1 =
            node.x + node.selfW / 2

        y1 =
            node.y + node.selfH

        kidEdges =
            List.concatMap
                (\(Placed k) ->
                    let
                        x2 =
                            k.x + k.selfW / 2

                        y2 =
                            k.y

                        mid =
                            (y1 + y2) / 2
                    in
                    Svg.path
                        [ SA.class "tree-edge"
                        , SA.d ("M" ++ num x1 ++ " " ++ num y1 ++ " C" ++ num x1 ++ " " ++ num mid ++ ", " ++ num x2 ++ " " ++ num mid ++ ", " ++ num x2 ++ " " ++ num y2)
                        , SA.stroke color
                        , SA.markerEnd ("url(#arr-" ++ String.fromInt pid ++ ")")
                        , SA.fill "none"
                        ]
                        []
                        :: drawEdges color pid (Placed k)
                )
                node.kids

        slotEdges =
            List.concatMap
                (\( sx, sy ) ->
                    let
                        mid =
                            (y1 + sy) / 2
                    in
                    [ Svg.path
                        [ SA.class "tree-edge"
                        , SA.d ("M" ++ num x1 ++ " " ++ num y1 ++ " C" ++ num x1 ++ " " ++ num mid ++ ", " ++ num sx ++ " " ++ num mid ++ ", " ++ num sx ++ " " ++ num (sy - 12))
                        , SA.stroke color
                        , SA.strokeDasharray "3 3"
                        , SA.opacity "0.45"
                        , SA.fill "none"
                        ]
                        []
                    , Svg.circle [ SA.class "socket-ghost", SA.cx (num sx), SA.cy (num sy), SA.r "11" ] []
                    , Svg.text_ [ SA.class "socket-label", SA.x (num sx), SA.y (num (sy + 3)), SA.textAnchor "middle" ] [ Svg.text "SLOT" ]
                    ]
                )
                node.slots
    in
    kidEdges ++ slotEdges


drawNodes : Game -> Player -> Bool -> Bool -> Maybe String -> Placed -> List (Svg Msg)
drawNodes game player interactive compact selected (Placed node) =
    let
        f =
            factionOf player

        click =
            if interactive || node.key /= "patron" then
                [ Svg.Events.onClick (Select node.key) ]

            else
                []

        self =
            if node.key == "patron" then
                Svg.g ([ SA.class ("node-g patron" ++ (if selected == Just "patron" then " selected" else "")) ] ++ click)
                    [ Svg.rect [ SA.class "node-bg", SA.x (fnum node.x), SA.y (fnum node.y), SA.width (fnum node.selfW), SA.height (fnum node.selfH), SA.rx "8" ] []
                    , Svg.rect [ SA.x (fnum node.x), SA.y (fnum node.y), SA.width (fnum node.selfW), SA.height "4", SA.rx "2", SA.fill f.color ] []
                    , Svg.text_ [ SA.class "node-sub", SA.x (fnum (node.x + 10)), SA.y (fnum (node.y + 18)) ] [ Svg.text "PATRON" ]
                    , Svg.text_ [ SA.class "node-title", SA.x (fnum (node.x + 10)), SA.y (fnum (node.y + 38)) ] [ Svg.text f.name ]
                    , Svg.text_ [ SA.class "node-num", SA.x (fnum (node.x + 10)), SA.y (fnum (node.y + 58)) ] [ Svg.text ("P " ++ String.fromInt f.power ++ "  I " ++ String.fromInt f.income ++ "  $ " ++ String.fromInt player.money ++ "  C " ++ String.fromInt player.compute) ]
                    ]

            else
                case Engine.findInstance game node.key of
                    Just g ->
                        let
                            card =
                                Engine.cardOf g

                            lines =
                                wrapWords card.name 18 |> List.take 2
                        in
                        Svg.g ([ SA.class ("node-g" ++ (if selected == Just node.key then " selected" else "")) ] ++ click)
                            ([ Svg.rect [ SA.class "node-bg", SA.x (fnum node.x), SA.y (fnum node.y), SA.width (fnum node.selfW), SA.height (fnum node.selfH), SA.rx "7" ] []
                             , Svg.rect [ SA.x (fnum node.x), SA.y (fnum node.y), SA.width (fnum node.selfW), SA.height "4", SA.rx "2", SA.fill (typeOf card.type_).color ] []
                             , Svg.text_ [ SA.class "node-sub", SA.x (fnum (node.x + 10)), SA.y (fnum (node.y + 16)) ] [ Svg.text ((typeOf card.type_).label ++ (if g.disabledTurns > 0 then " · DOWN" else "")) ]
                             ]
                                ++ List.indexedMap (\i line -> Svg.text_ [ SA.class "node-title", SA.x (fnum (node.x + 10)), SA.y (fnum (node.y + 32 + toFloat i * 14)), SA.fontSize "13" ] [ Svg.text line ]) lines
                                ++ [ Svg.text_ [ SA.class "node-num", SA.x (fnum (node.x + 10)), SA.y (fnum (node.y + node.selfH - 12)) ] [ Svg.text ("P " ++ String.fromInt (displayPower g) ++ "  R " ++ String.fromInt (resistance g) ++ "  $ " ++ String.fromInt g.money ++ (if g.privilegedTurns > 0 then "  PRIV" else "")) ]
                                   ]
                            )

                    Nothing ->
                        Svg.g [] []
    in
    self :: List.concatMap (drawNodes game player interactive compact selected) node.kids



-- Modals


viewModal : Model -> Html Msg
viewModal model =
    case model.modal of
        NoModal ->
            text ""

        Help ->
            shell model True (helpBody CloseModal)

        EventModal ->
            shell model False (eventBody model)

        AttackModal form ->
            shell model False (attackBody model form False False)

        Rolling form ->
            shell model False (attackBody model form True False)

        RollResult form ->
            shell model False (attackBody model form False True)

        VictoryModal ->
            shell model True (victoryBody model)

        ReorgModal id parent ->
            shell model False (reorgBody model id parent)

        Confirm title body yes ->
            shell model False
                (div []
                    [ h2 [] [ text title ]
                    , p [ class "lead" ] [ text body ]
                    , div [ class "btn-row", style "justify-content" "flex-end" ]
                        [ button [ class "btn btn-ghost", onClick CloseModal ] [ text "Cancel" ]
                        , button [ class "btn btn-danger", onClick yes ] [ text "Release" ]
                        ]
                    ]
                )

        CardSheet id ->
            shell model False (cardSheet model id)


shell : Model -> Bool -> Html Msg -> Html Msg
shell model wide inner =
    div [ class "modal-back", classList [ ( "sheet-back", phone model ) ], onClick Backdrop ]
        [ div
            [ class "modal"
            , classList [ ( "wide", wide && not (phone model) ), ( "sheet", phone model ) ]
            , A.attribute "role" "dialog"
            , stopPropagationOn "click" (Decode.succeed ( NoOp, True ))
            ]
            [ inner ]
        ]


helpBody : Msg -> Html Msg
helpBody close =
    div []
        [ h2 [] [ text "How to play" ]
        , p [ class "lead" ] [ text "A satirical game of the last five years before the frontier closes. Original rules and wording." ]
        , div [ class "help-body help-cols" ] [ rulesBody ]
        , div [ class "btn-row", style "margin-top" "16px", style "justify-content" "flex-end" ]
            [ button [ class "btn btn-primary", onClick close ] [ text "Close" ] ]
        ]


rulesBody : Html Msg
rulesBody =
    div []
        [ div []
            [ h3 [] [ text "The window" ]
            , ul []
                [ li [] [ text "The game runs from 2026 to 2031, one round per quarter — 20 rounds. If nobody has won when 2031 arrives, the closest patron is scored." ]
                , li [] [ text "You play one hidden faction against two or three AI rivals. Each faction has a secret goal. Anyone can also win the public way: control 6 settled groups at once." ]
                ]
            , h3 [] [ text "The structure" ]
            , ul []
                [ li [] [ text "Your patron has a treasury and three outgoing control sockets. Groups hang from sockets, drawn as arrows. A group with its own sockets can hold subordinates, so trees grow more than one level deep." ]
                , li [] [ text "Control steals a whole branch. Neutralize and Destroy send subordinates back to the open field. A newly taken group is privileged for four turns: it cannot be attacked, and it pays no income or compute, until that privilege ends. Every secret goal that counts groups counts only settled groups." ]
                ]
            , h3 [] [ text "A turn" ]
            , ul []
                [ li [] [ text "At the start of your turn you collect income: the patron's income, plus each group's, paid onto that group. Compute-producing groups add Compute." ]
                , li [] [ text "You have two actions. Only one of them may be an attack, whether the roll succeeds or fails. The other can lean on a group (2 money, +2 on your next attack against it, up to +6), release a group, reorganize it, attempt a weights leak, or pass." ]
                , li [] [ text "A new headline is drawn at the start of each quarter." ]
                ]
            ]
        , div []
            [ h3 [] [ text "Attacks" ]
            , ul []
                [ li [] [ Html.strong [] [ text "Control" ], text " takes a group and its branch. You need a free socket. You only get one attack each turn, so a second Control, Neutralize, or Destroy waits until next turn." ]
                , li [] [ Html.strong [] [ text "Neutralize" ], text " breaks a group free of its owner. " ]
                , li [] [ Html.strong [] [ text "Destroy" ], text " removes a group from the game." ]
                , li [] [ text "Roll 2d6. You succeed if the total is less than or equal to your attack strength, and is not a 12. A 12 always fails." ]
                , li [] [ text "Strength = attacker power + transfer from the parent + 2 per dollar spent (up to 4 dollars) + any lean you have banked on that group + alignment and special modifiers − resistance − the defender's parent − the defender's spend − 2, because every attack is contested." ]
                ]
            , h3 [] [ text "Alignments" ]
            , ul []
                [ li [] [ text "Corporate and Open are opposites, Safety and Accelerationist are opposites, Nationalist and Chaotic are opposites. Academic stands alone." ]
                , li [] [ text "For Control, a shared alignment helps and an opposite hurts (±3). For Neutralize and Destroy, that is reversed." ]
                ]
            , h3 [] [ text "The rest" ]
            , ul []
                [ li [] [ text "On a phone, use the tabs to move between your tree, rivals, the open field, the log, and these rules." ]
                , li [] [ text "The same seed always deals the same game. Add ?autoplay=1&seed=N to watch every seat, or call window.game from the console." ]
                , li [] [ text disclaimer ]
                ]
            ]
        ]


eventBody : Model -> Html Msg
eventBody model =
    case model.game |> Maybe.andThen .lastEvent of
        Nothing ->
            div [] [ p [] [ text "The headline fades." ], button [ class "btn", onClick CloseModal ] [ text "Continue" ] ]

        Just ev ->
            div []
                [ div [ class "k", style "letter-spacing" "0.16em", style "color" "var(--violet)" ] [ text ("HEADLINE · " ++ ev.when) ]
                , h2 [] [ text ev.name ]
                , p [ class "lead" ] [ text ev.flavor ]
                , div [ class "btn-row", style "justify-content" "flex-end" ]
                    [ button [ class "btn btn-primary", onClick CloseModal ] [ text "Continue" ] ]
                ]


attackBody : Model -> AttackForm -> Bool -> Bool -> Html Msg
attackBody model form spinning done =
    case model.game of
        Nothing ->
            text ""

        Just game ->
            case Engine.findInstance game form.targetId of
                Nothing ->
                    div [] [ p [] [ text "That group is gone." ], button [ class "btn", onClick CloseModal ] [ text "Close" ] ]

                Just target ->
                    let
                        card =
                            Engine.cardOf target

                        you =
                            Engine.getPlayer game 0
                    in
                    if spinning || done then
                        let
                            roll =
                                Maybe.withDefault { a = 1, b = 1, sum = 2, strength = 0, success = False, kind = "", targetName = "", attackerName = "" } game.lastRoll
                        in
                        div []
                            [ div [ class "k" ] [ text (Engine.kindKey form.kind) ]
                            , h2 [] [ text card.name ]
                            , div [ class "dice-row" ]
                                [ die (if spinning then 3 else roll.a) spinning
                                , die (if spinning then 4 else roll.b) spinning
                                , div []
                                    [ div [ class "roll-sum" ] [ text (if spinning then "—" else String.fromInt roll.sum) ]
                                    , div [ class ("roll-res" ++ (if spinning then "" else if roll.success then " ok" else " fail")) ]
                                        [ text
                                            (if spinning then
                                                "ROLLING"

                                             else if roll.success then
                                                "SUCCESS"

                                             else
                                                "FAILS"
                                            )
                                        ]
                                    ]
                                ]
                            , if spinning then
                                text ""

                              else
                                div [ class "btn-row", style "justify-content" "flex-end" ]
                                    [ button [ class "btn btn-primary", onClick CloseModal ] [ text "Continue" ] ]
                            ]

                    else
                        let
                            usable =
                                (PatronAtk :: List.map (\g -> GroupAtk g.instanceId) you.grove)
                                    |> List.filter
                                        (\atk ->
                                            case Engine.canAttack game you atk target form.kind of
                                                Ok _ ->
                                                    True

                                                Err _ ->
                                                    False
                                        )

                            money =
                                Engine.totalMoney you

                            spend =
                                min (min money Engine.maxAttackSpend) form.spend

                            br =
                                Engine.attackStrength game 0 form.attacker target.instanceId form.kind spend 0

                            parents =
                                Engine.freeParents you
                        in
                        if List.isEmpty usable then
                            div []
                                [ h2 [] [ text ((Engine.kindKey form.kind) ++ " " ++ card.name) ]
                                , p [ class "lead" ] [ text "No legal attacker." ]
                                , button [ class "btn", onClick CloseModal ] [ text "Close" ]
                                ]

                        else
                            div []
                                [ div [ class "k" ] [ text ((Engine.kindKey form.kind) ++ " attack") ]
                                , h2 [] [ text card.name ]
                                , p [ class "lead" ] [ text (card.flavor ++ (if card.parody then " (A parody card. Satire, not a factual claim.)" else "")) ]
                                , div [ class "attacker-picks" ]
                                    (List.map
                                        (\atk ->
                                            let
                                                label =
                                                    case atk of
                                                        PatronAtk ->
                                                            (factionOf you).name ++ " (patron)"

                                                        GroupAtk iid ->
                                                            case Engine.findInstance game iid of
                                                                Just g ->
                                                                    (Engine.cardOf g).name

                                                                Nothing ->
                                                                    "Group"
                                            in
                                            button [ classList [ ( "on", atk == form.attacker ) ], onClick (SetAttacker atk) ] [ text label ]
                                        )
                                        usable
                                    )
                                , if form.kind == Control then
                                    div [ class "parent-picks" ]
                                        (List.map
                                            (\parent ->
                                                button [ classList [ ( "on", parent == form.parent ) ], onClick (SetParent parent) ] [ text (parentLabel you parent) ]
                                            )
                                            parents
                                          )

                                  else
                                    text ""
                                , div [ class "formula" ]
                                    [ div [] [ text "Attacker power ", Html.strong [] [ text (String.fromInt br.atkP) ] ]
                                    , div [] [ text "Transfer from parent ", Html.strong [] [ text (signed br.transfer) ] ]
                                    , div [] [ text ("Money spent ($" ++ String.fromInt br.spend ++ ") "), Html.strong [] [ text (signed (br.spend * Engine.spendWeight)) ] ]
                                    , div [] [ text "Lean banked ", Html.strong [] [ text (signed br.lean) ] ]
                                    , div [] [ text "Alignment ", Html.strong [] [ text (signed br.aMod) ] ]
                                    , div [] [ text "Special ", Html.strong [] [ text (signed br.special) ] ]
                                    , div [] [ text "Resistance ", Html.strong [] [ text ("−" ++ String.fromInt br.res) ] ]
                                    , div [] [ text "Defender's parent ", Html.strong [] [ text ("−" ++ String.fromInt br.defT) ] ]
                                    , div [] [ text "Contested ", Html.strong [] [ text "−2" ] ]
                                    , div [] [ text ("Need " ++ String.fromInt (max 2 br.strength) ++ " or less on 2d6 · a 12 always fails") ]
                                    ]
                                , div [ class "spend-row" ]
                                    [ label [ class "field", style "flex" "1" ]
                                        [ text "Spend"
                                        , input [ type_ "range", A.min "0", A.max (String.fromInt (min money Engine.maxAttackSpend)), value (String.fromInt spend), onInput SetSpend ] []
                                        ]
                                    , div [ class "odds" ]
                                        [ text (String.fromInt (round (br.prob * 100)) ++ "%")
                                        , span [] [ text ("$ " ++ String.fromInt spend ++ " of " ++ String.fromInt money) ]
                                        ]
                                    ]
                                , p [ class "defender-note" ] [ text "The defender may spend money after you confirm, which can lower these odds. A single attack can spend at most 4 dollars, and each dollar adds 2 strength." ]
                                , div [ class "btn-row", style "justify-content" "flex-end" ]
                                    [ button [ class "btn btn-ghost", onClick CloseModal ] [ text "Cancel" ]
                                    , button [ class "btn btn-primary", onClick Launch ] [ text "Roll 2d6" ]
                                    ]
                                ]


reorgBody : Model -> String -> Parent -> Html Msg
reorgBody model id current =
    case model.game of
        Nothing ->
            text ""

        Just game ->
            case Engine.findInstance game id of
                Nothing ->
                    div [] [ p [] [ text "Gone." ], button [ class "btn", onClick CloseModal ] [ text "Close" ] ]

                Just g ->
                    let
                        you =
                            Engine.getPlayer game 0

                        parents =
                            List.filter
                                (\parent ->
                                    parent /= Under id && g.parent /= Just parent && not (List.any (\d -> Under d.instanceId == parent) (subtree you id)) && List.member parent (Engine.freeParents you)
                                )
                                (Patron :: List.map (\x -> Under x.instanceId) you.grove)
                    in
                    if List.isEmpty parents then
                        div []
                            [ h2 [] [ text ("Reorganize " ++ (Engine.cardOf g).name) ]
                            , p [ class "lead" ] [ text "No free legal socket to move to." ]
                            , button [ class "btn", onClick CloseModal ] [ text "Close" ]
                            ]

                    else
                        div []
                            [ h2 [] [ text ("Reorganize " ++ (Engine.cardOf g).name) ]
                            , p [ class "lead" ] [ text "Move this group, and its subordinates, to a free socket." ]
                            , div [ class "parent-picks" ]
                                (List.map (\parent -> button [ classList [ ( "on", parent == current ) ], onClick (SetParent parent) ] [ text (parentLabel you parent) ]) parents)
                            , div [ class "btn-row", style "justify-content" "flex-end" ]
                                [ button [ class "btn btn-ghost", onClick CloseModal ] [ text "Cancel" ]
                                , button [ class "btn btn-primary", onClick DoReorg ] [ text "Move" ]
                                ]
                            ]


victoryBody : Model -> Html Msg
victoryBody model =
    case model.game of
        Nothing ->
            text ""

        Just game ->
            let
                winner =
                    Maybe.map (\id -> Engine.getPlayer game id) game.winner

                wf =
                    Maybe.map factionOf winner
            in
            div []
                [ div [ class "victory" ]
                    [ div [ class "k" ] [ text (if Maybe.map .isAI winner == Just False then "You close the window" else "The window closes") ]
                    , h2 [ style "color" (Maybe.map .color wf |> Maybe.withDefault "#e0b44a") ] [ text (Maybe.map .name wf |> Maybe.withDefault "No one") ]
                    , p [] [ text (Maybe.withDefault "" game.winReason) ]
                    ]
                , table [ class "scoreboard" ]
                    [ thead [] [ tr [] [ th [] [ text "Patron" ], th [] [ text "Groups" ], th [] [ text "Compute" ], th [] [ text "Money" ], th [] [ text "Goal" ], th [] [ text "Secret" ] ] ]
                    , tbody []
                        (List.map
                            (\p ->
                                let
                                    f =
                                        factionOf p

                                    g =
                                        Engine.goalProgress game p
                                in
                                tr [ classList [ ( "winner", game.winner == Just p.id ) ] ]
                                    [ td [ class "fn", A.attribute "data-k" "Patron", style "color" f.color ] [ text f.name ]
                                    , td [ A.attribute "data-k" "Groups" ] [ text (String.fromInt (Engine.groupCount p)) ]
                                    , td [ A.attribute "data-k" "Compute" ] [ text (String.fromInt p.compute) ]
                                    , td [ A.attribute "data-k" "Money" ] [ text (String.fromInt (Engine.totalMoney p)) ]
                                    , td [ A.attribute "data-k" "Goal" ] [ text (String.fromInt (round (g.secretRatio * 100)) ++ "%") ]
                                    , td [ A.attribute "data-k" "Secret" ] [ text f.goalText ]
                                    ]
                            )
                            game.players
                        )
                    ]
                , case game.lastRoll of
                    Just roll ->
                        p [ class "lead" ] [ text ("Last roll " ++ String.fromInt roll.a ++ "+" ++ String.fromInt roll.b ++ "=" ++ String.fromInt roll.sum ++ (if roll.success then " — success." else " — fails.")) ]

                    Nothing ->
                        text ""
                , div [ class "btn-row victory-actions", style "justify-content" "center" ]
                    [ button [ class "btn btn-ghost", disabled (not model.actionsReady), onClick ReviewBoard ] [ text "Review the board" ]
                    , button [ class "btn", disabled (not model.actionsReady), onClick Rematch ] [ text "Same chairs, new seed" ]
                    , button [ class "btn btn-primary", disabled (not model.actionsReady), onClick GotoFaction ] [ text "New Game" ]
                    ]
                ]


cardSheet : Model -> String -> Html Msg
cardSheet model id =
    case model.game of
        Nothing ->
            text ""

        Just game ->
            if id == "patron" then
                let
                    viewer =
                        Engine.getPlayer game model.viewingPlayer

                    vf =
                        factionOf viewer

                    goal =
                        Engine.goalProgress game viewer
                in
                div [ class "card-sheet" ]
                    [ div [ class "cs-type", style "color" vf.color ] [ text "Patron" ]
                    , h2 [] [ text vf.name ]
                    , p [ class "lead" ] [ text vf.motto ]
                    , p [ class "flavor" ] [ text vf.flavor ]
                    , if viewer.id == 0 || game.gameOver then
                        div [ class "goal-box" ] [ Html.strong [] [ text "Secret goal" ], text vf.goalText, div [ class "goal-detail" ] [ text goal.secretText ] ]

                      else
                        p [ class "lead" ] [ text "Their secret goal stays hidden." ]
                    , button [ class "btn btn-primary", onClick CloseModal ] [ text "Close" ]
                    ]

            else
                case Engine.findInstance game id of
                    Nothing ->
                        div [] [ p [] [ text "That group is gone." ], button [ class "btn", onClick CloseModal ] [ text "Close" ] ]

                    Just g ->
                        let
                            card =
                                Engine.cardOf g

                            owner =
                                case g.ownerId of
                                    Just oid ->
                                        (factionOf (Engine.getPlayer game oid)).name

                                    Nothing ->
                                        "Uncontrolled"
                        in
                        div [ class "card-sheet" ]
                            [ div [ class "cs-type" ] [ text ((typeOf card.type_).label ++ (if card.parody then " · PARODY" else "")) ]
                            , h2 [] [ text card.name ]
                            , p [ class "lead" ] [ text (card.title ++ " · " ++ owner) ]
                            , div [ class "pills" ] (pills (Engine.alignmentsOf g))
                            , p [ class "flavor" ] [ text card.flavor ]
                            , div [ class "btn-row" ] (attackButtons game (Just id) ++ [ button [ class "btn btn-ghost", onClick CloseModal ] [ text "Close" ] ])
                            ]



-- Small helpers


factionOf : Player -> Faction
factionOf p =
    factionById p.factionId


typeOf : String -> TypeMeta
typeOf key =
    List.filter (\( k, _ ) -> k == key) typeMeta
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault { label = key, color = "#8a8696" }


alignMeta : String -> Alignment
alignMeta key =
    List.filter (\( k, _ ) -> k == key) alignments
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault { color = "#888", letter = "?" }


pills : List String -> List (Html Msg)
pills xs =
    List.map (\a -> span [ class "pill", style "--pc" (alignMeta a).color ] [ text a ]) xs


nums : Group -> Maybe Player -> List (Html Msg)
nums g owner =
    let
        card =
            Engine.cardOf g

        inc =
            case owner of
                Just p ->
                    -- income helper is not exported; show card income
                    card.income

                Nothing ->
                    card.income
    in
    [ span [] [ text "P ", Html.b [] [ text (String.fromInt (displayPower g)) ] ]
    , span [] [ text "R ", Html.b [] [ text (String.fromInt (resistance g)) ] ]
    , span [] [ text "I ", Html.b [] [ text (String.fromInt inc) ] ]
    , span [] [ text "$ ", Html.b [] [ text (String.fromInt g.money) ] ]
    ]


displayPower : Group -> Int
displayPower g =
    let
        card =
            Engine.cardOf g

        p =
            card.power + g.powerMod
    in
    if g.disabledTurns > 0 then
        max 0 (p - 2)

    else
        max 0 p


resistance : Group -> Int
resistance g =
    let
        r =
            (Engine.cardOf g).resistance
    in
    if g.disabledTurns > 0 then
        max 1 (r - 1)

    else
        r


meter : String -> String -> Html Msg
meter k v =
    div [ class "meter" ] [ div [ class "k" ] [ text k ], div [ class "v" ] [ text v ] ]


meterBox : String -> String -> String -> Html Msg
meterBox kind k v =
    div [ class ("meter " ++ kind) ] [ div [ class "k" ] [ text k ], div [ class "v" ] [ text v ] ]


inlineMeter : String -> String -> String -> Html Msg
inlineMeter kind k v =
    span [ class ("meter " ++ kind) ] [ span [ class "k" ] [ text k ], span [ class "v" ] [ text v ] ]


parentLabel : Player -> Parent -> String
parentLabel player parent =
    case parent of
        Patron ->
            (factionOf player).short

        Under iid ->
            case List.filter (\g -> g.instanceId == iid) player.grove of
                g :: _ ->
                    (Engine.cardOf g).name

                [] ->
                    "—"


subtree : Player -> String -> List Group
subtree player iid =
    case List.filter (\g -> g.instanceId == iid) player.grove of
        g :: _ ->
            g :: List.concatMap (\c -> subtree player c.instanceId) (List.filter (\c -> c.parent == Just (Under iid)) player.grove)

        [] ->
            []


die : Int -> Bool -> Html Msg
die n spinning =
    div [ class "die", classList [ ( "spin", spinning ) ], attribute "data-v" (String.fromInt n) ]
        (List.repeat 9 (Html.i [] []))


cardArt : Card -> Html Msg
cardArt card =
    let
        col =
            (typeOf card.type_).color
    in
    svg [ SA.viewBox "0 0 168 44", SA.width "100%", SA.height "44", A.attribute "aria-hidden" "true" ]
        [ Svg.rect [ SA.width "168", SA.height "44", SA.fill col, SA.opacity "0.35" ] []
        , Svg.circle [ SA.cx "84", SA.cy "22", SA.r "10", SA.fill "none", SA.stroke "#f3ecd8", SA.strokeWidth "1.2" ] []
        ]


computeRace : Game -> String
computeRace game =
    game.players
        |> List.map (\p -> (factionById p.factionId).short ++ " " ++ String.fromInt p.compute)
        |> String.join " · "


reasonText : Result String a -> String
reasonText result =
    case result of
        Err reason ->
            reason

        Ok _ ->
            ""


signed : Int -> String
signed n =
    if n >= 0 then
        "+" ++ String.fromInt n

    else
        String.fromInt n


num : Float -> String
num n =
    String.fromFloat n


fnum : Float -> String
fnum =
    num


wrapWords : String -> Int -> List String
wrapWords text maxLen =
    let
        step word ( lines, cur ) =
            let
                next =
                    if cur == "" then
                        word

                    else
                        cur ++ " " ++ word
            in
            if String.length next > maxLen && cur /= "" then
                ( lines ++ [ cur ], word )

            else
                ( lines, next )

        ( done, last ) =
            List.foldl step ( [], "" ) (String.words text)
    in
    if last == "" then
        done

    else
        done ++ [ last ]
