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


-- MODEL


type alias SimulatedPlayer =
    { health : Int
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
            SimulatedPlayer 100

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
                    ( 3250, 1400 )

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


simulateMatchup : SimulatedMatchup -> Generator SimulatedMatchup
simulateMatchup ( simMe, simYou ) =
    let
        me =
            simMe.player

        myReward =
            me.primary
                |> Maybe.Extra.or me.secondary
                |> Maybe.map Equipment.stats
                |> Maybe.map .killReward
                |> Maybe.withDefault 0

        wonMe =
            { me | money = me.money + myReward }

        wonSimMe =
            { simMe | player = wonMe }

        deadSimMe =
            { simMe | health = 0 }

        you =
            simYou.player

        yourReward =
            you.primary
                |> Maybe.Extra.or you.secondary
                |> Maybe.map Equipment.stats
                |> Maybe.map .killReward
                |> Maybe.withDefault 0

        wonYou =
            { you | money = you.money + yourReward }

        wonSimYou =
            { simYou | player = wonYou }

        deadSimYou =
            { simYou | health = 0 }
    in
        choice
            ( wonSimMe, deadSimYou )
            ( deadSimMe, wonSimYou )


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
