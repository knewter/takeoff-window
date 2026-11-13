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
    | Drawer String


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
    , hintSeen : Bool
    , bannerKey : Maybe String
    , stripMore : Bool
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
    | DoInvoke String
    | AskDrop String
    | DoDrop String
    | OpenReorg String
    | DoReorg
    | ShowVictory
    | Rematch
    | ShowHelp
    | ShowEvent
    | DismissHint
    | DismissBanner
    | OpenGoal
    | ToggleStrip
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
    , hintSeen = False
    , bannerKey = Nothing
    , stripMore = False
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
        , eventBanner model game
        , turnHint model
        , div [ class "board" ]
            [ div [ class "col" ]
                [ youCard you game False
                , raceList game
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
                , div [ class "market" ] (marketCards game you model.selectedId)
                ]
            , div [ class "col col-right" ]
                [ div [ class "panel-head" ]
                    [ span [] [ text "Rivals" ]
                    , span [] [ text "click a name to view their tree" ]
                    ]
                , div [ class "rivals" ] (rivalCards model game)
                ]
            , actionBar model game
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
                    div [ class "tree-stage" ]
                        [ youLine you
                        , viewBanner viewer
                        , div [ class "tree-mobile" ]
                            [ div [ class "tree-scroller" ] [ treeSvg game viewer (viewer.id == 0 && not game.gameOver && not model.busy) True (landscape model) model.selectedId ]
                            , treeList game viewer model.selectedId
                            ]
                        ]

                TabRivals ->
                    div []
                        [ raceList game
                        , div [ class "panel-head" ] [ span [] [ text "Rivals" ], span [] [ text "Tap a name to view their tree" ] ]
                        , div [ class "rivals" ] (rivalCards model game)
                        ]

                TabMarket ->
                    div []
                        [ div [ class "panel-head" ]
                            [ span [] [ text "The open field" ]
                            , span [] [ text (String.fromInt (List.length game.uncontrolled) ++ " in play · " ++ String.fromInt (List.length game.groupDeck) ++ " in the deck") ]
                            ]
                        , div [ class "market" ] (marketCards game you model.selectedId)
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
        , eventBanner model game
        , turnHint model
        , div [ class "phone-play" ]
            [ div [ class "phone-stage" ] [ stage ]
            , actionBar model game
            ]
        , tabBar model.tab model.modal
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


youLine : Player -> Html Msg
youLine you =
    let
        f =
            factionOf you
    in
    div [ class "you-line" ]
        [ span [ class "you-line-name", style "color" f.color ] [ text f.short ]
        , span [] [ text ("$" ++ String.fromInt (Engine.totalMoney you)) ]
        , span [] [ text ("C" ++ String.fromInt you.compute) ]
        , span [] [ text (String.fromInt (Engine.groupCount you) ++ " groups") ]
        , button [ class "goal-open inline", onClick OpenGoal, A.title "Open your goal" ] [ text "Goal" ]
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
        , button [ class "goal-open", onClick OpenGoal, A.title "Open your goal" ] [ text "Goal" ]
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
            , heliosNudge you
            , div [ class "goal-detail" ] [ text ("Public win: " ++ goal.publicText ++ ".") ]
            , div [ class "progress" ] [ Html.i [ style "width" pct, style "background" f.color ] [] ]
            ]
        , if compact then
            text ""

          else
            p [ class "ability" ] [ text f.ability ]
        ]


reorgExplain : String
reorgExplain =
    "Reorganize moves your group under the patron or another of your groups with a free socket, to open sockets, raise attack/defense transfer, or spread risk across branches."


turnHint : Model -> Html Msg
turnHint model =
    if model.hintSeen || model.screen /= Play then
        text ""

    else
        div [ class "turn-hint" ]
            [ p []
                [ text "You have two actions. Only one may be an attack — Control, Neutralize, or Destroy — whether it hits or misses. Lean spends $2 to bank +2 strength on a group, then strike it later. A group you just took is privileged for 4 turns and does not count toward a goal until it settles." ]
            , button [ class "btn btn-sm", onClick DismissHint ] [ text "Got it" ]
            ]


actionBar : Model -> Game -> Html Msg
actionBar model game =
    let
        you =
            Engine.getPlayer game 0

        yourTurn =
            not game.gameOver && (Engine.current game).id == 0 && not model.busy && not model.driveAll

        selected =
            case model.selectedId of
                Just id ->
                    Engine.findInstance game id

                Nothing ->
                    Nothing

        offers =
            actionOffers game you yourTurn selected
    in
    div [ class "action-bar strip dock" ]
        [ div [ class "strip-status" ]
            [ span [ class "strip-count" ]
                [ text
                    (if yourTurn then
                        String.fromInt game.actionsLeft ++ "/2"

                     else
                        "—"
                    )
                ]
            , span
                [ class "strip-money"
                , A.title
                    (if not yourTurn then
                        "Waiting"

                     else if you.attackedThisTurn then
                        "Attack used"

                     else
                        "Attack free"
                    )
                ]
                [ text ("$" ++ String.fromInt (Engine.totalMoney you)) ]
            ]
        , div [ class "strip-chips" ]
            [ div [ class "strip-verbs" ] (visibleVerbs model offers)
            , div [ class "strip-tail" ] (tailChips game you yourTurn selected offers model)
            ]
        ]


leanNudge : Player -> Bool -> Game -> Html Msg
leanNudge you yourTurn _ =
    if yourTurn && you.attackedThisTurn && Engine.totalMoney you >= 2 then
        p [ class "lean-nudge" ] [ text "You can still Lean on a target for next turn." ]

    else
        text ""


selectedActions : Game -> Player -> Maybe Group -> List Offer -> Html Msg
selectedActions game you selected offers =
    let
        about =
            List.filter (\o -> o.band == "attack" || o.label == "Lean" || o.label == "Reorganize" || o.label == "Release") offers
    in
    div [ class "action-selected" ]
        [ div [ class "action-band-label" ] [ text (selectedTitle game you selected) ]
        , div [ class "action-picks" ]
            (if selected == Nothing || modeless selected then
                [ span [ class "hint" ] [ text "Select a group to see the actions that apply, with odds and cost." ] ]

             else
                List.map offerRow about
            )
        ]


modeless : Maybe Group -> Bool
modeless selected =
    selected == Nothing


selectedTitle : Game -> Player -> Maybe Group -> String
selectedTitle game you selected =
    case selected of
        Nothing ->
            "No group selected"

        Just g ->
            let
                where_ =
                    case g.ownerId of
                        Nothing ->
                            "open field"

                        Just oid ->
                            if oid == you.id then
                                "your group"

                            else
                                (factionOf (Engine.getPlayer game oid)).short ++ "'s group"
            in
            "On " ++ (Engine.cardOf g).name ++ " · " ++ where_


type alias Offer =
    { label : String
    , band : String
    , enabled : Bool
    , reason : String
    , act : Msg
    , tip : String
    }


verbList : List Offer -> List Offer
verbList offers =
    List.filter
        (\o ->
            o.enabled
                && (o.band == "attack" || o.band == "special" || o.label == "Lean" || o.label == "Reorganize" || o.label == "Release" || o.label == "Leak Weights")
        )
        offers


visibleVerbs : Model -> List Offer -> List (Html Msg)
visibleVerbs model offers =
    let
        verbs =
            verbList offers

        shown =
            if List.length verbs <= 2 then
                verbs

            else
                List.take 2 verbs

        more =
            if List.length verbs <= 2 then
                []

            else
                [ button [ class "chip", onClick ToggleStrip, A.title "More actions" ] [ text "More" ] ]

        extra =
            if model.stripMore then
                div [ class "strip-more" ] (List.map chip (List.drop 2 verbs))

            else
                text ""
    in
    extra :: List.map chip shown ++ more


tailChips : Game -> Player -> Bool -> Maybe Group -> List Offer -> Model -> List (Html Msg)
tailChips game you yourTurn selected offers model =
    let
        hint =
            if not yourTurn then
                if game.gameOver then
                    "Window closed"

                else
                    "Waiting"

            else if selected == Nothing then
                "Select a group"

            else if you.attackedThisTurn then
                "Attack used"

            else
                ""

        pass =
            List.filter (\o -> o.label == "Pass" && o.enabled) offers
    in
    (if hint == "" then
        []

     else
        [ span [ class "strip-hint", A.title hint ] [ text hint ] ]
    )
        ++ List.map chip pass
        ++ (if game.gameOver then
                [ button [ class "chip primary", onClick ShowVictory ] [ text "Outcome" ] ]

            else if model.busy then
                [ button [ class "chip", onClick SpeedUp ] [ text "Faster" ] ]

            else
                []
           )
        ++ [ button [ class "chip ghost", onClick OpenGoal, A.title "Your goal and the compute race" ] [ text "Goal" ] ]


shortLabel : String -> String
shortLabel label =
    case label of
        "Control" ->
            "Ctrl"

        "Destroy" ->
            "Dstr"

        "Neutralize" ->
            "Neut"

        "Reorganize" ->
            "Move"

        "Release" ->
            "Drop"

        "Remove founder" ->
            "Cut"

        "Recruit" ->
            "Join"

        "Leak Weights" ->
            "Leak"

        "Ration" ->
            "Rate"

        "Swing" ->
            "Wild"

        _ ->
            label


chip : Offer -> Html Msg
chip offer =
    button
        [ class "chip"
        , classList [ ( "primary", offer.enabled && offer.band == "attack" ), ( "off", not offer.enabled ) ]
        , disabled (not offer.enabled)
        , A.title (if offer.tip == "" then offer.reason else offer.reason ++ " — " ++ offer.tip)
        , onClick offer.act
        ]
        [ text (shortLabel offer.label) ]


offerRow : Offer -> Html Msg
offerRow offer =
    chip offer


invokeOffers : Game -> Player -> Bool -> List Offer
invokeOffers game you yourTurn =
    if not yourTurn then
        []

    else
        List.filterMap (\g -> invokeOffer you g) you.grove


invokeOffer : Player -> Group -> Maybe Offer
invokeOffer you g =
    let
        card =
            Engine.cardOf g

        base label cost tip =
            if g.privilegedTurns > 0 then
                Just { label = label, band = "special", enabled = False, reason = "Privileged for now", act = NoOp, tip = tip }

            else if g.exhausted then
                Just { label = label, band = "special", enabled = False, reason = "Already used this turn", act = NoOp, tip = tip }

            else if Engine.totalMoney you < cost then
                Just { label = label, band = "special", enabled = False, reason = "Need $" ++ String.fromInt cost, act = NoOp, tip = tip }

            else
                Just { label = label, band = "special", enabled = True, reason = "Spend $" ++ String.fromInt cost, act = DoInvoke g.instanceId, tip = tip }
    in
    case card.special of
        Just "doomer-cult" ->
            base "Recruit" 1 "A doomer cult recruits. A rival lab loses 1 power, and safety pressure ticks. Parody, not fact."

        Just "accel-cult" ->
            base "Recruit" 1 "An accelerationist cult poaches. You gain 1 compute and a rival lab loses 1 power. Parody, not fact."

        Just "remove-founder" ->
            base "Remove founder" 3 "Cut a founder loose from their patron for 3. A briefing leaks and the service is exposed. Parody, not fact."

        Just "ration-compute" ->
            base "Ration" 0 "The chip kingpin moves one compute toward you and off the rival with the most. Parody, not fact."

        Just "wildcard-swing" ->
            base "Swing" 0 "A coin-flip quarter. Either a windfall, or a stumble. Parody, not fact."

        _ ->
            Nothing


actionOffers : Game -> Player -> Bool -> Maybe Group -> List Offer
actionOffers game you yourTurn selected =
    [ attackOffer game you yourTurn "Control" Control selected
    , attackOffer game you yourTurn "Neutralize" Neutralize selected
    , attackOffer game you yourTurn "Destroy" Destroy selected
    , leanOffer game you yourTurn selected
    , reorgOffer you yourTurn selected
    , releaseOffer you yourTurn selected
    ,         leakOffer game you yourTurn
    , passOffer game yourTurn
    ]
        ++ invokeOffers game you yourTurn


attackOffer : Game -> Player -> Bool -> String -> Kind -> Maybe Group -> Offer
attackOffer game you yourTurn label kind selected =
    let
        idle =
            { label = label, band = "attack", enabled = False, reason = "", act = NoOp, tip = "" }
    in
    if not yourTurn then
        { idle | reason = if game.gameOver then "Window closed" else "Waiting on a rival" }

    else if you.attackedThisTurn then
        { idle | reason = "Already attacked this turn" }

    else
        case selected of
            Nothing ->
                { idle
                    | reason =
                        if kind == Neutralize then
                            "Select a rival's group"

                        else
                            "Select a group first"
                }

            Just g ->
                if g.ownerId == Just you.id then
                    { idle
                        | reason =
                            if kind == Control then
                                "Already yours"

                            else
                                "Release it instead"
                    }

                else if kind == Neutralize && g.ownerId == Nothing then
                    { idle | reason = "Already in the open field" }

                else
                    case firstAttacker game you g kind of
                        Just atk ->
                            let
                                spend =
                                    Engine.bestValueSpend game you atk g kind

                                br =
                                    Engine.attackStrength game you.id atk g.instanceId kind spend 0
                            in
                            { idle
                                | enabled = True
                                , reason = String.fromInt (round (br.prob * 100)) ++ "% at $" ++ String.fromInt spend
                                , act = OpenAttack g.instanceId kind
                            }

                        Nothing ->
                            { idle | reason = shortReason (reasonText (Engine.canAttack game you PatronAtk g kind)) }


leanOffer : Game -> Player -> Bool -> Maybe Group -> Offer
leanOffer game you yourTurn selected =
    let
        idle =
            { label = "Lean", band = "setup", enabled = False, reason = "", act = NoOp, tip = "Spend $2 to bank +2 strength on your next attack against this group, up to +6." }
    in
    if not yourTurn then
        { idle | reason = if game.gameOver then "Window closed" else "Waiting on a rival" }

    else
        case selected of
            Nothing ->
                { idle
                    | reason =
                        if Engine.totalMoney you < 2 then
                            "Need $2"

                        else
                            "Select a group first"
                }

            Just g ->
                if g.ownerId == Just you.id then
                    { idle | reason = "Not on your own group" }

                else if Engine.totalMoney you < 2 then
                    { idle | reason = "Need $2" }

                else if Engine.pressureOn you g.instanceId >= 6 then
                    { idle | reason = "Already at +6" }

                else
                    { idle
                        | enabled = True
                        , reason = "Spend $2 · bank +" ++ String.fromInt (min 6 (Engine.pressureOn you g.instanceId + 2))
                        , act = DoLean g.instanceId
                    }


reorgOffer : Player -> Bool -> Maybe Group -> Offer
reorgOffer you yourTurn selected =
    let
        idle =
            { label = "Reorganize", band = "setup", enabled = False, reason = "", act = NoOp, tip = reorgExplain }
    in
    if not yourTurn then
        { idle | reason = "Waiting on a rival" }

    else
        case selected of
            Just g ->
                if g.ownerId == Just you.id && not (List.isEmpty (reorgParents you g)) then
                    { idle | enabled = True, reason = "Open a socket, raise transfer, or spread risk", act = OpenReorg g.instanceId }

                else if g.ownerId == Just you.id then
                    { idle | reason = "No free socket to move to" }

                else
                    { idle | reason = "Select one of your groups" }

            Nothing ->
                { idle | reason = "Select one of your groups" }


releaseOffer : Player -> Bool -> Maybe Group -> Offer
releaseOffer you yourTurn selected =
    let
        idle =
            { label = "Release", band = "setup", enabled = False, reason = "", act = NoOp, tip = "Return this group and its branch to the open field." }
    in
    if not yourTurn then
        { idle | reason = "Waiting on a rival" }

    else
        case selected of
            Just g ->
                if g.ownerId == Just you.id then
                    { idle | enabled = True, reason = "Send it and its branch back to the open field", act = AskDrop g.instanceId }

                else
                    { idle | reason = "Select one of your groups" }

            Nothing ->
                { idle | reason = "Select one of your groups" }


leakOffer : Game -> Player -> Bool -> Offer
leakOffer game you yourTurn =
    let
        idle =
            { label = "Leak Weights", band = "setup", enabled = False, reason = "", act = NoOp, tip = "Open Wave spends $4 to try to leak the weights. Succeeds on 10 or less." }
    in
    if you.factionId /= "open-wave" then
        { idle | reason = "Open Wave only" }

    else if game.flags.weightsLeak then
        { idle | reason = "Already leaked" }

    else if not yourTurn then
        { idle | reason = if game.gameOver then "Window closed" else "Waiting on a rival" }

    else if not (List.any (\g -> (Engine.cardOf g).type_ == "lab") you.grove) then
        { idle | reason = "Need a lab you control" }

    else if Engine.totalMoney you < Engine.leakCost then
        { idle | reason = "Need $4" }

    else
        { idle | enabled = True, reason = "Spend $4 · succeeds on 10 or less", act = DoLeak }


passOffer : Game -> Bool -> Offer
passOffer game yourTurn =
    if yourTurn then
        { label = "Pass", band = "turn", enabled = True, reason = "Ends the turn", act = PassTurn, tip = "Pass ends the turn." }

    else
        { label = "Pass", band = "turn", enabled = False, reason = if game.gameOver then "Window closed" else "Waiting on a rival", act = NoOp, tip = "Pass ends the turn." }


firstAttacker : Game -> Player -> Group -> Kind -> Maybe Attacker
firstAttacker game you target kind =
    let
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
    in
    if List.member PatronAtk usable then
        Just PatronAtk

    else
        List.head usable


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


marketCards : Game -> Player -> Maybe String -> List (Html Msg)
marketCards game you selected =
    if List.isEmpty game.uncontrolled then
        [ div [ class "empty-tree" ] [ text "The open field is empty. The deck is exhausted." ] ]

    else
        List.map (\g -> groupCard g (selected == Just g.instanceId) Nothing (Engine.pressureOn you g.instanceId)) game.uncontrolled


groupCard : Group -> Bool -> Maybe Player -> Int -> Html Msg
groupCard g selected owner lean =
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
            , specialTag card.special
            , div [ class "title" ] [ text card.title ]
            , div [ class "pills" ] (pills (Engine.alignmentsOf g))
            , div [ class "nums" ] (nums g owner lean)
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
                    , threatMeter game p
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
                                        [ text (card.name ++ leanMark (Engine.pressureOn (Engine.getPlayer game 0) g.instanceId)) ]
                                )
                                p.grove
                        )
                    ]
            )


eventBanner : Model -> Game -> Html Msg
eventBanner model game =
    case game.lastEvent of
        Just ev ->
            let
                key =
                    ev.id ++ "@" ++ ev.when
            in
            if model.bannerKey == Just key then
                text ""

            else
                div [ class "event-banner" ]
                    [ button [ class "banner-x", onClick DismissBanner, A.title "Hide headline" ] [ text "×" ]
                    , div [ class "banner-copy", onClick ShowEvent ]
                        [ div [ class "k" ] [ text ("Headline · " ++ ev.when) ]
                        , h3 [] [ text ev.name ]
                        ]
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


tabBar : Tab -> Modal -> Html Msg
tabBar current modal =
    nav [ class "tabbar" ]
        (List.map
            (\( id, label ) ->
                button [ classList [ ( "on", current == id || tabLit modal id ) ], onClick (SetTab id) ] [ text label ]
            )
            [ ( TabTree, "Tree" ), ( TabRivals, "Rivals" ), ( TabMarket, "Open" ), ( TabLog, "Log" ), ( TabRules, "Rules" ) ]
        )


tabLit : Modal -> Tab -> Bool
tabLit modal tab =
    case ( modal, tab ) of
        ( Drawer "log", TabLog ) ->
            True

        ( Drawer "rules", TabRules ) ->
            True

        _ ->
            False


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
                        [ text ("P " ++ String.fromInt (displayPower g) ++ " · R " ++ String.fromInt (resistance g) ++ " · $ " ++ String.fromInt g.money ++ leanMark (Engine.pressureOn (Engine.getPlayer game 0) g.instanceId)) ]
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
                                ++ [ Svg.text_ [ SA.class "node-num", SA.x (fnum (node.x + 10)), SA.y (fnum (node.y + node.selfH - 12)) ] [ Svg.text ("P " ++ String.fromInt (displayPower g) ++ "  R " ++ String.fromInt (resistance g) ++ "  $ " ++ String.fromInt g.money ++ (if g.privilegedTurns > 0 then "  PRIV" else "") ++ leanMark (Engine.pressureOn (Engine.getPlayer game 0) g.instanceId)) ]
                                   ]
                            )

                    Nothing ->
                        Svg.g [] []
    in
    self :: List.concatMap (drawNodes game player interactive compact selected) node.kids



-- Modals


raceList : Game -> Html Msg
raceList game =
    div [ class "race" ]
        (div [ class "race-k" ] [ text "Public win · 6 settled groups" ]
            :: List.map
                (\p ->
                    let
                        f =
                            factionOf p

                        prog =
                            Engine.goalProgress game p

                        pct =
                            String.fromFloat (prog.publicRatio * 100) ++ "%"
                    in
                    div [ class "race-row", classList [ ( "you", p.id == 0 ) ] ]
                        [ span [ class "race-name", style "color" f.color ] [ text f.short ]
                        , span [ class "race-track" ] [ Html.i [ style "width" pct, style "background" f.color ] [] ]
                        , span [ class "race-n" ] [ text (String.fromInt (round (prog.publicRatio * toFloat Engine.publicGoal)) ++ "/" ++ String.fromInt Engine.publicGoal) ]
                        ]
                )
                game.players
        )


threatMeter : Game -> Player -> Html Msg
threatMeter game p =
    let
        prog =
            Engine.goalProgress game p

        pct =
            String.fromFloat (prog.publicRatio * 100) ++ "%"
    in
    div [ class "threat", A.title "Settled groups toward the public win of 6" ]
        [ span [] [ text "Public" ]
        , span [ class "race-track" ] [ Html.i [ style "width" pct ] [] ]
        , span [] [ text (String.fromInt (round (prog.publicRatio * toFloat Engine.publicGoal)) ++ "/6") ]
        ]


drawerLog : Game -> Html Msg
drawerLog game =
    div []
        [ h2 [] [ text "Turn log" ]
        , div [ class "log" ] (logEntries game)
        ]


drawerGoal : Game -> Html Msg
drawerGoal game =
    let
        you =
            Engine.getPlayer game 0

        f =
            factionOf you
    in
    div []
        [ div [ class "cs-type", style "color" f.color ] [ text "Your patron" ]
        , h2 [] [ text f.name ]
        , p [ class "lead" ] [ text f.motto ]
        , p [] [ text f.goalText ]
        , p [] [ text (Engine.goalProgress game you).secretText ]
        , heliosNudge you
        , p [ class "ability" ] [ text f.ability ]
        , raceList game
        ]


viewModal : Model -> Html Msg
viewModal model =
    case model.modal of
        NoModal ->
            text ""

        Help ->
            shell model (helpBody CloseModal) (text "")

        EventModal ->
            shell model (eventBody model) (text "")

        AttackModal form ->
            let
                parts =
                    attackBody model form False False
            in
            shell model parts.body parts.foot

        Rolling form ->
            let
                parts =
                    attackBody model form True False
            in
            shell model parts.body parts.foot

        RollResult form ->
            let
                parts =
                    attackBody model form False True
            in
            shell model parts.body parts.foot

        VictoryModal ->
            shell model (victoryBody model) (text "")

        ReorgModal id parent ->
            shell model (reorgBody model id parent) (text "")

        Confirm title body yes ->
            shell model
                (div []
                    [ h2 [] [ text title ]
                    , p [ class "lead" ] [ text body ]
                    ]
                )
                (div [ class "btn-row", style "justify-content" "flex-end" ]
                    [ button [ class "btn btn-ghost", onClick CloseModal ] [ text "Cancel" ]
                    , button [ class "btn btn-danger", onClick yes ] [ text "Release" ]
                    ]
                )

        CardSheet id ->
            shell model (cardSheet model id) (text "")

        Drawer "log" ->
            case model.game of
                Just game ->
                    shell model (drawerLog game) (text "")

                Nothing ->
                    text ""

        Drawer "rules" ->
            shell model (div [] [ h2 [] [ text "How to play" ], div [ class "help-body" ] [ rulesBody ] ]) (text "")

        Drawer "goal" ->
            case model.game of
                Just game ->
                    shell model (drawerGoal game) (text "")

                Nothing ->
                    text ""

        Drawer _ ->
            text ""


shell : Model -> Html Msg -> Html Msg -> Html Msg
shell model body foot =
    div
        [ class "modal-back sheet-back"
        , A.attribute "data-sheet" ""
        , A.attribute "data-sheet-dismiss" (if canDismiss model.modal then "1" else "0")
        , A.attribute "data-sheet-open" (openSnap model.modal)
        , onClick Backdrop
        ]
        [ div
            [ class "modal sheet"
            , classList [ ( "sheet-wide", not (phone model) ) ]
            , A.attribute "data-sheet-panel" ""
            , A.attribute "data-snap" "half"
            , A.attribute "role" "dialog"
            , stopPropagationOn "click" (Decode.succeed ( NoOp, True ))
            ]
            [ button [ class "sheet-handle", A.attribute "data-sheet-handle" "", A.attribute "aria-label" "Drag sheet", A.type_ "button" ] []
            , div [ class "sheet-body", A.attribute "data-sheet-body" "" ] [ body ]
            , div [ class "sheet-foot" ] [ foot ]
            , button [ class "sheet-close", A.attribute "data-sheet-close" "", A.attribute "hidden" "", onClick CloseModal ] [ text "Close" ]
            ]
        ]


canDismiss : Modal -> Bool
canDismiss modal =
    case modal of
        VictoryModal ->
            False

        Rolling _ ->
            False

        _ ->
            True


openSnap : Modal -> String
openSnap modal =
    case modal of
        AttackModal _ ->
            "full"

        ReorgModal _ _ ->
            "full"

        VictoryModal ->
            "full"

        RollResult _ ->
            "full"

        _ ->
            "half"


kindLabel : Kind -> String
kindLabel kind =
    case kind of
        Control ->
            "Control"

        Neutralize ->
            "Neutralize"

        Destroy ->
            "Destroy"


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
                , li [] [ text "You have two actions, shown in the action bar as Actions left. Only one of them may be an attack, whether the roll succeeds or fails. The bar marks Attack used: yes after that. The other action can be Lean, Reorganize, Release, a weights leak, or Pass." ]
                , li [] [ text "Lean spends 2 money to bank +2 strength on your next attack against that group, up to +6. Banked lean is printed on the group. After you attack, you can still lean for next turn." ]
                , li [] [ text reorgExplain ]
                , li [] [ text "Release returns a group and its branch to the open field. Leak Weights is Open Wave only: spend 4, and succeed on 10 or less if you hold a lab. Pass ends the turn." ]
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
                [ li [] [ text "Helios labs and chipmakers pay Compute on the last privileged turn. Their money, and every other group's income, still waits the full four." ]
                , li [] [ text "Hold a cult to Recruit, the Listening Service to Remove a founder, the chip kingpin to Ration, or the wildcard to Swing. A plant delay skips anyone who already holds power. Parody, not fact." ]
                , li [] [ text "On a phone, use the tabs to move between your tree, rivals, the open field, the log, and these rules. The action bar stays on screen." ]
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


attackBody : Model -> AttackForm -> Bool -> Bool -> { body : Html Msg, foot : Html Msg }
attackBody model form spinning done =
    case model.game of
        Nothing ->
            { body = text "", foot = text "" }

        Just game ->
            case Engine.findInstance game form.targetId of
                Nothing ->
                    { body = p [] [ text "That group is gone." ]
                    , foot = button [ class "btn", onClick CloseModal ] [ text "Close" ]
                    }

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
                        { body =
                            div []
                            [ div [ class "k" ] [ text (kindLabel form.kind) ]
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
                            ]
                        , foot =
                            if spinning then
                                text ""

                            else
                                button [ class "btn btn-primary", onClick CloseModal ] [ text "Continue" ]
                        }

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
                            { body =
                                div []
                                    [ h2 [] [ text (kindLabel form.kind ++ " " ++ card.name) ]
                                    , p [ class "lead" ] [ text "No legal attacker." ]
                                    ]
                            , foot = button [ class "btn", onClick CloseModal ] [ text "Close" ]
                            }

                        else
                            { body =
                                div []
                                [ h2 [] [ text (kindLabel form.kind) ]
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
                                , oddsLadder game you form.attacker target form.kind (min money Engine.maxAttackSpend) spend
                                , p [ class "defender-note" ] [ text "The defender may spend money after you confirm, which can lower these odds. A single attack can spend at most 4 dollars, and each dollar adds 2 strength." ]
                                ]
                            , foot =
                                div [ class "btn-row", style "justify-content" "flex-end" ]
                                    [ button [ class "btn btn-ghost", onClick CloseModal ] [ text "Cancel" ]
                                    , button [ class "btn btn-primary", onClick Launch ] [ text "Roll 2d6" ]
                                    ]
                            }


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
                            reorgParents you g
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
                            , p [ class "lead" ] [ text reorgExplain ]
                            , div [ class "reorg-options" ]
                                (List.map
                                    (\parent ->
                                        button
                                            [ class "reorg-option"
                                            , classList [ ( "on", parent == current ) ]
                                            , onClick (SetParent parent)
                                            ]
                                            [ span [ class "offer-name" ] [ text (parentLabel you parent) ]
                                            , div [ class "reorg-lines" ] (List.map (\line -> span [] [ text line ]) (reorgPreview game you g parent))
                                            ]
                                    )
                                    parents
                                )
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


nums : Group -> Maybe Player -> Int -> List (Html Msg)
nums g owner lean =
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
        ++ (if lean > 0 then
                [ span [ class "lean-bank" ] [ text ("Lean +" ++ String.fromInt lean) ] ]

            else
                []
           )


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


reorgParents : Player -> Group -> List Parent
reorgParents you g =
    List.filter
        (\parent ->
            parent /= Under g.instanceId && g.parent /= Just parent && not (List.any (\d -> Under d.instanceId == parent) (subtree you g.instanceId)) && List.member parent (Engine.freeParents you)
        )
        (Patron :: List.map (\x -> Under x.instanceId) you.grove)


reorgPreview : Game -> Player -> Group -> Parent -> List String
reorgPreview game you g parent =
    let
        support =
            case parent of
                Patron ->
                    (factionOf you).power // 4

                Under iid ->
                    case Engine.findInstance game iid of
                        Just host ->
                            (Engine.cardOf host).transfer // 2

                        Nothing ->
                            0

        hostName =
            parentLabel you parent

        leaving =
            case g.parent of
                Just old ->
                    if old /= parent then
                        let
                            urgent =
                                List.isEmpty (Engine.freeParents you)
                        in
                        [ "Frees a socket on " ++ parentLabel you old ++ (if urgent then " so you can Control again" else "") ]

                    else
                        []

                Nothing ->
                    []

        moved =
            List.length (subtree you g.instanceId)

        risk =
            case parent of
                Patron ->
                    "Branch risk: nothing sits above it, so a neutralize drops this branch only (" ++ String.fromInt moved ++ (if moved == 1 then " group)." else " groups).")

                Under _ ->
                    "Branch risk: neutralizing " ++ hostName ++ " would also drop " ++ String.fromInt moved ++ (if moved == 1 then " group." else " groups.")
    in
    [ "Attack +" ++ String.fromInt support ++ " from parent transfer"
    , "Defense +" ++ String.fromInt support ++ " vs rivals"
    ]
        ++ leaving
        ++ [ risk ]


oddsLadder : Game -> Player -> Attacker -> Group -> Kind -> Int -> Int -> Html Msg
oddsLadder game you attacker target kind cap current =
    let
        step s =
            let
                br =
                    Engine.attackStrength game you.id attacker target.instanceId kind s 0
            in
            span
                [ class "odds-step"
                , classList [ ( "on", s == current ) ]
                ]
                [ text ("$" ++ String.fromInt s ++ " " ++ String.fromInt (round (br.prob * 100)) ++ "%") ]

        zero =
            Engine.attackStrength game you.id attacker target.instanceId kind 0 0

        now =
            Engine.attackStrength game you.id attacker target.instanceId kind current 0
    in
    div [ class "odds-ladder" ]
        [ div [ class "odds-steps" ] (List.map step (List.range 0 cap))
        , p [ class "odds-jump" ]
            [ text ("Odds jump from " ++ String.fromInt (round (zero.prob * 100)) ++ "% at $0 to " ++ String.fromInt (round (now.prob * 100)) ++ "% at $" ++ String.fromInt current ++ ".") ]
        ]


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


heliosNudge : Player -> Html Msg
heliosNudge you =
    if you.factionId /= "helios" then
        text ""

    else
        let
            away =
                max 0 (Engine.agiCompute - you.compute)

            hasLab =
                List.any (\g -> (Engine.cardOf g).type_ == "lab" && g.privilegedTurns <= 0) you.grove

            hasChip =
                List.any (\g -> (Engine.cardOf g).type_ == "chipmaker" && g.privilegedTurns <= 0) you.grove

            awayStack =
                max 0 (12 - you.compute)
        in
        p [ class "helios-nudge" ]
            [ text
                (if you.compute >= Engine.agiCompute then
                    "You have the compute to take off."

                 else if hasLab && hasChip && awayStack == 0 then
                    "You have a settled lab, a settled chipmaker, and 12 compute."

                 else if hasLab && hasChip then
                    "You're " ++ String.fromInt awayStack ++ " compute away from the lab and chipmaker win, and " ++ String.fromInt away ++ " away from takeoff."

                 else
                    "You're " ++ String.fromInt away ++ " compute away from takeoff."
                )
            ]


specialTag : Maybe String -> Html Msg
specialTag special =
    case special of
        Just "doomer-cult" ->
            span [ class "special-tag", A.title "Recruit: spend 1. A rival lab loses 1 power and safety pressure ticks. Parody, not fact." ] [ text "Cult" ]

        Just "accel-cult" ->
            span [ class "special-tag", A.title "Recruit: spend 1. Gain 1 compute and a rival lab loses 1 power. Parody, not fact." ] [ text "Cult" ]

        Just "remove-founder" ->
            span [ class "special-tag", A.title "Remove founder: spend 3 to cut a founder from their patron. A briefing leaks. Parody, not fact." ] [ text "Agency" ]

        Just "ration-compute" ->
            span [ class "special-tag", A.title "Ration: move 1 compute toward you. Rivals' chips also yield 1 less while you hold this. Parody, not fact." ] [ text "Ration" ]

        Just "wildcard-swing" ->
            span [ class "special-tag", A.title "Swing: a coin-flip quarter, windfall or stumble. Parody, not fact." ] [ text "Wildcard" ]

        Just "founder-hunter" ->
            span [ class "special-tag", A.title "Stronger against founder groups." ] [ text "Hunter" ]

        _ ->
            text ""


leanMark : Int -> String
leanMark n =
    if n > 0 then
        "  Lean +" ++ String.fromInt n

    else
        ""


reasonText : Result String a -> String
reasonText result =
    case result of
        Err reason ->
            reason

        Ok _ ->
            ""


shortReason : String -> String
shortReason reason =
    if String.contains "already launched" reason then
        "Already attacked this turn"

    else if String.contains "free control" reason || String.contains "no free" (String.toLower reason) then
        "No free socket"

    else if String.contains "privileged" reason then
        "Privileged for now"

    else if String.contains "already control" reason then
        "Already yours"

    else if String.contains "already neutral" reason then
        "Already in the open field"

    else if String.contains "drop it" reason then
        "Release it instead"

    else if reason == "" then
        "Not legal"

    else
        reason


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
