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
    ( SimulatedPlayer, SimulatedPlayer )


type alias SimulationState =
    ( SimulatedTeam, SimulatedTeam )


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
        ( simTeam us, simTeam them )


breakState : SimulationState -> ( Team, Team )
breakState ( simUs, simThem ) =
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
        ( breakTeam (Tuple.first rewards) simUs, breakTeam (Tuple.second rewards) simThem )


playerIfAlive : SimulatedPlayer -> Maybe SimulatedPlayer
playerIfAlive p =
    if p.health > 0 then
        Just p
    else
        Nothing


getMatchup : SimulationState -> ( Int, Int ) -> Maybe SimulatedMatchup
getMatchup ( ourTeam, theirTeam ) ( u, t ) =
    let
        me =
            Array.get u ourTeam.players
                |> Maybe.andThen playerIfAlive

        you =
            Array.get t theirTeam.players
                |> Maybe.andThen playerIfAlive
    in
        Maybe.map2 (,) me you


simulateTick : SimulatedMatchup -> Generator SimulatedMatchup
simulateTick ( simMe, simYou ) =
    let
        me =
            simMe.player

        you =
            simYou.player

        myHealth =
            simMe.health

        yourHealth =
            simYou.health

        myOldCooldown =
            simMe.cooldownLeft

        yourOldCooldown =
            simYou.cooldownLeft

        myStats =
            me.primary
                |> Maybe.Extra.or me.secondary
                |> Maybe.map Equipment.stats
                |> Maybe.withDefault Equipment.emptyStats

        yourStats =
            you.primary
                |> Maybe.Extra.or you.secondary
                |> Maybe.map Equipment.stats
                |> Maybe.withDefault Equipment.emptyStats

        myDamage =
            myStats.baseDamage

        yourDamage =
            yourStats.baseDamage

        myWinCooldown =
            myStats.ticksBetweenShots

        yourWinCooldown =
            yourStats.ticksBetweenShots

        myLossCooldown =
            clamp 0 myOldCooldown (myOldCooldown - 1)

        yourLossCooldown =
            clamp 0 yourOldCooldown (yourOldCooldown - 1)

        myReward =
            if myDamage >= yourHealth then
                myStats.killReward
            else
                0

        yourReward =
            if yourDamage >= myHealth then
                yourStats.killReward
            else
                0

        wonMe =
            { me | money = me.money + myReward }

        wonSimMe =
            { simMe | player = wonMe, cooldownLeft = myWinCooldown }

        whiffSimMe =
            { simMe | cooldownLeft = myLossCooldown }

        lostSimMe =
            { simMe | health = myHealth - yourDamage, cooldownLeft = myLossCooldown }

        wonYou =
            { you | money = you.money + yourReward }

        wonSimYou =
            { simYou | player = wonYou, cooldownLeft = yourWinCooldown }

        whiffSimYou =
            { simYou | cooldownLeft = yourLossCooldown }

        lostSimYou =
            { simYou | health = yourHealth - myDamage, cooldownLeft = yourLossCooldown }

        iWin =
            if myOldCooldown > 0 then
                Nothing
            else
                Just ( wonSimMe, lostSimYou )

        youWin =
            if yourOldCooldown > 0 then
                Nothing
            else
                Just ( lostSimMe, wonSimYou )

        nobodyWins =
            ( whiffSimMe, whiffSimYou )

        choices =
            [ iWin, (Just nobodyWins), youWin ]
                |> List.filterMap identity
    in
        Random.List.choose choices
            |> map (\( result, _ ) -> (Maybe.withDefault nobodyWins result))


simulateMatchup : SimulatedMatchup -> Generator SimulatedMatchup
simulateMatchup state =
    let
        nextTick : Int -> SimulatedMatchup -> Generator SimulatedMatchup
        nextTick i ( simMe, simYou ) =
            if (i < 0) || (simMe.health <= 0) || (simYou.health <= 0) then
                constant ( simMe, simYou )
            else
                simulateTick ( simMe, simYou )
                    |> andThen (nextTick (i - 1))
    in
        nextTick (.ticksBetweenShots (Equipment.stats Equipment.AWP)) state


applyMatchup : SimulationState -> ( Int, Int ) -> SimulatedMatchup -> SimulationState
applyMatchup ( ourTeam, theirTeam ) ( u, t ) ( newSimMe, newSimYou ) =
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
        ( newOurTeam, newTheirTeam )


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


simulate : ( Team, Team ) -> Generator ( Team, Team )
simulate ( ourTeam, theirTeam ) =
    makeState ( ourTeam, runEnemyAI theirTeam )
        |> simulateStep
        |> andThen simulateStep
        |> andThen simulateStep
        |> andThen simulateStep
        |> andThen simulateStep
        |> map breakState
