module Simulation
    exposing
        ( simulate
        )

import Player exposing (Player)
import Equipment
import Team exposing (Team)
import Array exposing (Array)
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
        breakPlayer : SimulatedPlayer -> Player
        breakPlayer p =
            if p.health > 0 then
                p.player
            else
                Player.dead p.player

        breakTeam : SimulatedTeam -> Team
        breakTeam t =
            Team t.side (Array.map breakPlayer t.players)
    in
        ( breakTeam simUs, breakTeam simThem )


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

        wonMe =
            { me | money = me.money + 3000 }

        wonSimMe =
            { simMe | player = wonMe }

        deadSimMe =
            { simMe | health = 0 }

        you =
            simYou.player

        wonYou =
            { you | money = you.money + 3000 }

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


simulate : ( Team, Team ) -> Generator ( Team, Team )
simulate init =
    makeState init
        |> simulateStep
        |> andThen simulateStep
        |> andThen simulateStep
        |> andThen simulateStep
        |> andThen simulateStep
        |> map breakState
