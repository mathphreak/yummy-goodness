module Simulation
    exposing
        ( simulate
        )

import Player exposing (Player)
import Equipment exposing (Equipment)
import Team exposing (Team)
import Array exposing (Array)
import Maybe.Extra
import Random exposing (..)
import Random.Extra exposing (..)
import Random.List
import Tuple2


-- MODEL


type alias SimulatedPlayer =
    { health : Int
    , cooldownLeft : Int
    , player : Player
    }


type alias Log =
    List String


type alias SimulatedTeam =
    { side : Equipment.Side
    , players : Array SimulatedPlayer
    }


type alias SimulatedMatchup =
    ( SimulatedPlayer, SimulatedPlayer, Log )


type alias SimulationState =
    ( SimulatedTeam, SimulatedTeam, Log )


commingle : ( a, b ) -> ( x, y ) -> ( ( a, x ), ( b, y ) )
commingle ( a, b ) ( x, y ) =
    ( ( a, x ), ( b, y ) )


makeState : ( Team, Team ) -> SimulationState
makeState ( us, them ) =
    let
        simPlayer : Player -> SimulatedPlayer
        simPlayer =
            SimulatedPlayer 100 0

        simTeam : Team -> SimulatedTeam
        simTeam t =
            SimulatedTeam t.side (Array.map simPlayer t.players)
    in
        ( simTeam us, simTeam them, [ "The Round Started!" ] )


breakState : SimulationState -> ( Team, Team, Log )
breakState ( simUs, simThem, log ) =
    let
        breakPlayer : Int -> SimulatedPlayer -> Player
        breakPlayer bonus p =
            let
                player =
                    p.player
            in
                if p.health > 0 then
                    { player | money = player.money + bonus }
                else
                    Player.dead { player | money = player.money + bonus }

        countAlive : SimulatedTeam -> Int
        countAlive team =
            team.players
                |> Array.filter (\p -> p.health > 0)
                |> Array.length

        rewards =
            case (compare (countAlive simUs) (countAlive simThem)) of
                GT ->
                    ( 3250, 1400 )

                EQ ->
                    case simUs.side of
                        Equipment.CT ->
                            ( 3250, 1400 )

                        Equipment.T ->
                            ( 1400, 3250 )

                LT ->
                    ( 1400, 3250 )

        breakTeam : Int -> SimulatedTeam -> Team
        breakTeam reward t =
            Team t.side (Array.map (breakPlayer reward) t.players) Array.empty
    in
        ( breakTeam (Tuple.first rewards) simUs, breakTeam (Tuple.second rewards) simThem, log )


playerIfAlive : SimulatedPlayer -> Maybe SimulatedPlayer
playerIfAlive p =
    if p.health > 0 then
        Just p
    else
        Nothing


getMatchup : SimulationState -> ( Int, Int ) -> Maybe SimulatedMatchup
getMatchup ( ourTeam, theirTeam, log ) ( u, t ) =
    let
        me =
            Array.get u ourTeam.players
                |> Maybe.andThen playerIfAlive

        you =
            Array.get t theirTeam.players
                |> Maybe.andThen playerIfAlive
    in
        Maybe.map3 (,,) me you (Just [])


simulateTick : SimulatedMatchup -> Generator SimulatedMatchup
simulateTick ( simMe, simYou, log ) =
    let
        me =
            simMe.player

        you =
            simYou.player

        my =
            Tuple.first

        your =
            Tuple.second

        simSelf =
            ( simMe, simYou )

        self =
            simSelf |> Tuple2.mapBoth .player

        health =
            simSelf |> Tuple2.mapBoth .health

        oldCooldown =
            simSelf |> Tuple2.mapBoth .cooldownLeft

        getGun : Player -> ( String, Equipment.WeaponStats )
        getGun p =
            p.secondary
                |> Maybe.Extra.or p.primary
                |> Maybe.map (\e -> ( Equipment.toString e, Equipment.stats e ))
                |> Maybe.withDefault ( "Nothing", Equipment.emptyStats )

        gun =
            self |> Tuple2.mapBoth getGun

        stats =
            gun |> Tuple2.mapBoth Tuple.second

        damage =
            stats |> Tuple2.mapBoth .baseDamage

        winCooldown =
            stats |> Tuple2.mapBoth .ticksBetweenShots

        lossCooldown =
            oldCooldown |> Tuple2.mapBoth (\c -> clamp 0 c (c - 1))

        calculateReward ( this, that, this2 ) =
            if (this damage) >= (that health) then
                .killReward (this2 stats)
            else
                0

        reward =
            ( ( my, your, my ), ( your, my, your ) )
                |> Tuple2.mapBoth calculateReward

        wonSelf =
            reward
                |> commingle self
                |> Tuple2.mapBoth (\( s, r ) -> { s | money = s.money + r })

        wonSim =
            wonSelf
                |> commingle winCooldown
                |> commingle simSelf
                |> Tuple2.mapBoth (\( s, ( c, p ) ) -> { s | player = p, cooldownLeft = c })

        whiffSim =
            simSelf
                |> commingle lossCooldown
                |> Tuple2.mapBoth (\( c, s ) -> { s | cooldownLeft = c })

        lostSim =
            commingle health (Tuple2.swap damage)
                |> commingle (commingle simSelf lossCooldown)
                |> Tuple2.mapBoth (\( ( sS, lC ), ( h, d ) ) -> { sS | health = h - d, cooldownLeft = lC })

        nobodyWins =
            ( my whiffSim, your whiffSim, log )

        iWin =
            if (my oldCooldown) > 0 then
                nobodyWins
            else
                ( my wonSim
                , your lostSim
                , (me.name ++ " shot " ++ you.name ++ " for " ++ (toString (my damage)) ++ " with " ++ (Tuple.first (my gun))) :: log
                )

        youWin =
            if (your oldCooldown) > 0 then
                nobodyWins
            else
                ( my lostSim
                , your wonSim
                , (you.name ++ " shot " ++ me.name ++ " for " ++ (toString (your damage)) ++ " with " ++ (Tuple.first (your gun))) :: log
                )

        choices =
            [ iWin, nobodyWins, youWin ]
    in
        Random.List.choose choices
            |> map (\( result, _ ) -> (Maybe.withDefault nobodyWins result))


simulateMatchup : SimulatedMatchup -> Generator SimulatedMatchup
simulateMatchup state =
    let
        nextTick : Int -> SimulatedMatchup -> Generator SimulatedMatchup
        nextTick i ( simMe, simYou, log ) =
            if (i < 0) || (simMe.health <= 0) || (simYou.health <= 0) then
                constant ( simMe, simYou, log )
            else
                simulateTick ( simMe, simYou, log )
                    |> andThen (nextTick (i - 1))
    in
        nextTick (.ticksBetweenShots (Equipment.stats Equipment.AWP)) state


applyMatchup : SimulationState -> ( Int, Int ) -> SimulatedMatchup -> SimulationState
applyMatchup ( ourTeam, theirTeam, log ) ( u, t ) ( newSimMe, newSimYou, newLog ) =
    let
        newUs =
            Array.set u newSimMe ourTeam.players

        newThem =
            Array.set t newSimYou theirTeam.players

        newOurTeam =
            { ourTeam | players = newUs }

        newTheirTeam =
            { theirTeam | players = newThem }
    in
        ( newOurTeam, newTheirTeam, newLog ++ log )


grabAndRunMatchup : SimulationState -> ( Int, Int ) -> Generator SimulationState
grabAndRunMatchup state indices =
    let
        simulateAndApply m =
            simulateMatchup m
                |> Random.map (applyMatchup state indices)
    in
        getMatchup state indices
            |> Maybe.map simulateAndApply
            |> Maybe.withDefault (constant state)


simulateStep : SimulationState -> Generator SimulationState
simulateStep state =
    pair (int 0 4) (int 0 4)
        |> andThen (grabAndRunMatchup state)


aiEnemyPurchase : Player -> Player
aiEnemyPurchase them =
    let
        purchaseIfPossible : Equipment -> Player -> Player
        purchaseIfPossible e p =
            if (Player.playerCanPurchaseEquipment p e) then
                Player.update (Player.Purchase e) p
            else
                p
    in
        them
            |> purchaseIfPossible Equipment.VestHelmet
            |> purchaseIfPossible Equipment.Vest
            |> purchaseIfPossible Equipment.AWP
            |> purchaseIfPossible Equipment.AK47
            |> purchaseIfPossible Equipment.M4A4
            |> purchaseIfPossible Equipment.XM1014
            |> purchaseIfPossible Equipment.Deagle
            |> purchaseIfPossible Equipment.Smoke


runEnemyAI : Team -> Team
runEnemyAI theirTeam =
    let
        players =
            theirTeam.players |> Array.map aiEnemyPurchase
    in
        { theirTeam | players = players }


simulate : ( Team, Team ) -> Generator ( Team, Team, Log )
simulate ( ourTeam, theirTeam ) =
    makeState ( ourTeam, runEnemyAI theirTeam )
        |> simulateStep
        |> andThen simulateStep
        |> andThen simulateStep
        |> andThen simulateStep
        |> andThen simulateStep
        |> map breakState
