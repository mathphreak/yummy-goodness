module Simulation exposing (simulate)

import Player exposing (Player)
import Equipment exposing (Equipment)
import Team exposing (Team)
import Array exposing (Array)
import Maybe.Extra
import List.Extra
import Random exposing (..)
import Random.Extra exposing (..)
import Random.List
import Tuple2
import Debug
import KillFeed exposing (KillFeed, KillFeedEntry)


-- MODEL


type alias SimulatedPlayer =
    { health : Int
    , cooldownLeft : Int
    , player : Player
    }


type alias SimulatedTeam =
    { side : Equipment.Side
    , players : Array SimulatedPlayer
    }


type alias SimulatedMatchup =
    ( SimulatedPlayer, SimulatedPlayer, Maybe KillFeedEntry )


type alias SimulationState =
    ( SimulatedTeam, SimulatedTeam, KillFeed )



{- I'm not sure if they give out trophies for Most Unnecessary Type Annotation,
   but if they do I think this should get one
-}


zip2 : ( a, b ) -> ( x, y ) -> ( ( a, x ), ( b, y ) )
zip2 ( a, b ) ( x, y ) =
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
        ( simTeam us, simTeam them, [] )


breakState : ( Equipment.Side, Int ) -> SimulationState -> ( Team, Team, KillFeed, Equipment.Side )
breakState ( lastWinner, winStreak ) ( simUs, simThem, feed ) =
    let
        breakPlayer : Int -> SimulatedPlayer -> Player
        breakPlayer bonus p =
            let
                player =
                    p.player
            in
                if p.health > 0 then
                    { player | money = clamp 0 16000 (player.money + bonus) }
                else
                    Player.dead { player | money = clamp 0 16000 (player.money + bonus) }

        countAlive : SimulatedTeam -> Int
        countAlive team =
            team.players
                |> Array.filter (\p -> p.health > 0)
                |> Array.length

        winningTeam =
            case (compare (countAlive simUs) (countAlive simThem)) of
                GT ->
                    simUs.side

                EQ ->
                    Equipment.CT

                LT ->
                    simThem.side

        breakTeam : SimulatedTeam -> Team
        breakTeam t =
            let
                reward =
                    if t.side == winningTeam then
                        3250
                    else if t.side == lastWinner then
                        1400
                    else
                        clamp 1400 3400 (1400 + 500 * winStreak)
            in
                Team t.side (Array.map (breakPlayer reward) t.players) Array.empty
    in
        ( breakTeam simUs, breakTeam simThem, feed, winningTeam )


playerIfAlive : SimulatedPlayer -> Maybe SimulatedPlayer
playerIfAlive p =
    if p.health > 0 then
        Just p
    else
        Nothing


getMatchup : SimulationState -> ( Int, Int ) -> Maybe SimulatedMatchup
getMatchup ( ourTeam, theirTeam, feed ) ( u, t ) =
    let
        me =
            Array.get u ourTeam.players
                |> Maybe.andThen playerIfAlive

        you =
            Array.get t theirTeam.players
                |> Maybe.andThen playerIfAlive
    in
        Maybe.map3 (,,) me you (Just Nothing)


simulateTick : SimulatedMatchup -> Generator SimulatedMatchup
simulateTick ( simMe, simYou, _ ) =
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

        getGun : Player -> Equipment
        getGun p =
            p.secondary
                |> Maybe.Extra.or p.primary
                |> Maybe.withDefault Equipment.Vest

        gun =
            self |> Tuple2.mapBoth getGun

        stats =
            gun |> Tuple2.mapBoth Equipment.stats

        hasArmor =
            self |> Tuple2.mapBoth Player.hasArmor

        calculateDamage ( stats, enemyHasArmor ) =
            if enemyHasArmor then
                round (toFloat stats.baseDamage * stats.armorPenetration / 100)
            else
                stats.baseDamage

        damage =
            Tuple2.swap hasArmor
                |> zip2 stats
                |> Tuple2.mapBoth calculateDamage

        firedCooldown =
            stats |> Tuple2.mapBoth .ticksBetweenShots

        heldFireCooldown =
            oldCooldown |> Tuple2.mapBoth (\c -> clamp 0 c (c - 1))

        calculateReward ( stats, ( damage, enemyHealth ) ) =
            if damage >= enemyHealth then
                stats.killReward
            else
                0

        reward =
            Tuple2.swap health
                |> zip2 damage
                |> zip2 stats
                |> Tuple2.mapBoth calculateReward

        firedSelf =
            reward
                |> zip2 self
                |> Tuple2.mapBoth (\( s, r ) -> { s | money = s.money + r })

        firedSim =
            firedSelf
                |> zip2 firedCooldown
                |> zip2 simSelf
                |> Tuple2.mapBoth (\( s, ( c, p ) ) -> { s | player = p, cooldownLeft = c })

        didNothingSim =
            simSelf
                |> zip2 heldFireCooldown
                |> Tuple2.mapBoth (\( c, s ) -> { s | cooldownLeft = c })

        hitSim =
            zip2 health (Tuple2.swap damage)
                |> zip2 (zip2 simSelf heldFireCooldown)
                |> Tuple2.mapBoth (\( ( sS, lC ), ( h, d ) ) -> { sS | health = h - d, cooldownLeft = lC })

        nothingHappens =
            ( my didNothingSim, your didNothingSim, Nothing )

        iHit =
            if (my oldCooldown) > 0 then
                nothingHappens
            else
                ( my firedSim
                , your hitSim
                , Just (KillFeedEntry me you (my gun))
                )

        iMissed =
            if (my oldCooldown) > 0 then
                nothingHappens
            else
                ( my firedSim
                , your didNothingSim
                , Nothing
                )

        youHit =
            if (your oldCooldown) > 0 then
                nothingHappens
            else
                ( my hitSim
                , your firedSim
                , Just (KillFeedEntry you me (your gun))
                )

        youMissed =
            if (your oldCooldown) > 0 then
                nothingHappens
            else
                ( my didNothingSim
                , your firedSim
                , Nothing
                )

        choices =
            [ iHit, iMissed, nothingHappens, youHit, youMissed ]
    in
        Random.List.choose choices
            |> map (\( result, _ ) -> (Maybe.withDefault nothingHappens result))


simulateMatchup : SimulatedMatchup -> Generator SimulatedMatchup
simulateMatchup state =
    let
        nextTick : Int -> SimulatedMatchup -> Generator SimulatedMatchup
        nextTick i ( simMe, simYou, kill ) =
            if (i < 0) || (simMe.health <= 0) || (simYou.health <= 0) then
                constant ( simMe, simYou, kill )
            else
                simulateTick ( simMe, simYou, kill )
                    |> andThen (nextTick (i - 1))
    in
        nextTick (.ticksBetweenShots (Equipment.stats Equipment.AWP)) state


applyMatchup : SimulationState -> ( Int, Int ) -> SimulatedMatchup -> SimulationState
applyMatchup ( ourTeam, theirTeam, feed ) ( u, t ) ( newSimMe, newSimYou, newKill ) =
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
        ( newOurTeam, newTheirTeam, feed ++ (Maybe.Extra.maybeToList newKill) )


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
            |> purchaseIfPossible Equipment.Deagle
            |> purchaseIfPossible Equipment.XM1014
            |> purchaseIfPossible Equipment.AK47
            |> purchaseIfPossible Equipment.M4A4
            |> purchaseIfPossible Equipment.AWP
            |> purchaseIfPossible Equipment.Smoke


runEnemyAI : Team -> Team
runEnemyAI theirTeam =
    let
        players =
            theirTeam.players |> Array.map aiEnemyPurchase
    in
        { theirTeam | players = players }


extractStreak : List Equipment.Side -> ( Equipment.Side, Int )
extractStreak winners =
    let
        rWinners =
            List.reverse winners

        lastWinner =
            List.head rWinners

        getStreak lW =
            rWinners
                |> List.Extra.takeWhile ((==) lW)
                |> List.length
    in
        case lastWinner of
            Just lW ->
                ( lW, getStreak lW )

            Nothing ->
                ( Equipment.CT, 0 )


simulate : ( Team, Team, List Equipment.Side ) -> Generator ( Team, Team, KillFeed, Equipment.Side )
simulate ( ourTeam, theirTeam, winners ) =
    let
        runStepsFrom n =
            List.repeat n (andThen simulateStep)
                |> List.foldr (>>) identity

        winningStreak =
            extractStreak winners
    in
        int 3 20
            |> andThen
                (\n ->
                    runStepsFrom n
                        (constant (makeState ( ourTeam, runEnemyAI theirTeam )))
                        |> map (breakState winningStreak)
                )
