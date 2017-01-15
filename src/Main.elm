module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlExtra exposing (..)
import Array exposing (Array)
import Player exposing (Player)
import Equipment
import Team exposing (Team)
import Random
import BotNames
import BuyMenu
import Simulation
import KillFeed exposing (KillFeed)


main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { us : Team
    , them : Team
    , selectedPlayer : Int
    , killFeed : KillFeed
    , roundWinners : List Equipment.Side
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        (Team.buildTeam Equipment.CT Array.empty)
        (Team.buildTeam Equipment.CT Array.empty)
        0
        []
        []
    , Random.generate RngLoad (Random.map2 (,) (BotNames.pickNames 10) Random.bool)
    )



-- UPDATE


type Msg
    = RngLoad ( Array String, Bool )
    | UsMsg Team.Msg
    | SelectPlayer Int
    | BeginSimulation
    | EndSimulation ( Team, Team, KillFeed, Equipment.Side )
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RngLoad ( names, weAreCT ) ->
            let
                ourSide =
                    if weAreCT then
                        Equipment.CT
                    else
                        Equipment.T

                ourNames =
                    Array.slice 0 5 names

                theirSide =
                    if weAreCT then
                        Equipment.T
                    else
                        Equipment.CT

                theirNames =
                    Array.slice 5 10 names
            in
                ( Model (Team.buildTeam ourSide ourNames) (Team.buildTeam theirSide theirNames) 0 [] []
                , Cmd.none
                )

        UsMsg msg ->
            let
                us =
                    model.us
            in
                ( { model | us = Team.update msg us }, Cmd.none )

        SelectPlayer i ->
            ( { model | selectedPlayer = i }, Cmd.none )

        BeginSimulation ->
            ( model, Random.generate EndSimulation (Simulation.simulate ( model.us, model.them, model.roundWinners )) )

        EndSimulation ( us, them, killFeed, winner ) ->
            ( { model
                | us = us
                , them = them
                , killFeed = killFeed
                , roundWinners = model.roundWinners ++ [ winner ]
              }
            , Cmd.none
            )

        Reset ->
            init



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


getSelectedPlayer : Model -> Maybe ( Int, Player )
getSelectedPlayer model =
    Array.get model.selectedPlayer model.us.players
        |> Maybe.map (\p -> ( model.selectedPlayer, p ))


historySummary : Equipment.Side -> List Equipment.Side -> List (Html Msg)
historySummary us winners =
    let
        classes =
            case us of
                Equipment.CT ->
                    ( "ct", "t" )

                Equipment.T ->
                    ( "t", "ct" )

        wins =
            winners
                |> List.filter ((==) us)
                |> List.length

        losses =
            (List.length winners) - wins
    in
        [ li [ class "summary" ]
            [ span [ class (Tuple.first classes) ] [ text (toString wins) ]
            , text "-"
            , span [ class (Tuple.second classes) ] [ text (toString losses) ]
            ]
        ]


showHistory : Equipment.Side -> List Equipment.Side -> List (Html Msg)
showHistory us winners =
    let
        textFor team =
            if team == us then
                ( "win", "W" )
            else
                ( "loss", "L" )

        elementFor i ( class_, letter ) =
            li [ class class_ ] [ span [] [ text letter ], small [] [ text (toString (i + 1)) ] ]
    in
        winners
            |> List.map textFor
            |> List.indexedMap elementFor


nextRoundButton : List Equipment.Side -> List (Html Msg)
nextRoundButton winners =
    if (List.length winners < 15) then
        [ li [ class "next" ]
            [ button [ type_ "button", onClick BeginSimulation ] [ text "NEXT" ]
            ]
        ]
    else
        []


historyPanel : Model -> Html Msg
historyPanel model =
    div [ class "history" ]
        [ ul [ class "rounds" ]
            ((historySummary model.us.side model.roundWinners)
                ++ (showHistory model.us.side model.roundWinners)
                ++ (nextRoundButton model.roundWinners)
            )
        ]


menuPanel : Model -> Html Msg
menuPanel model =
    let
        menu =
            case (getSelectedPlayer model) of
                Just ( i, p ) ->
                    [ BuyMenu.buyMenuFor (\a -> (UsMsg <| Team.PlayerMessage i <| a)) p ]

                Nothing ->
                    [ p [] [ text "Select a player!" ] ]

        actions =
            case (getSelectedPlayer model) of
                Just ( i, p ) ->
                    [ Player.actionsFor (\a -> (UsMsg <| Team.PlayerMessage i <| a)) p ]

                Nothing ->
                    []
    in
        div [ class "menus" ] (menu ++ actions)


uiPanel : Model -> Html Msg
uiPanel model =
    let
        pickUp =
            (\a -> UsMsg <| Team.PickUp model.selectedPlayer a)
    in
        div [ class "ui inGame" ]
            [ div [ class "us" ]
                [ h1 []
                    [ text "US ("
                    , span [ class (String.toLower (toString model.us.side)) ]
                        [ text (toString model.us.side) ]
                    , text ")"
                    ]
                , Team.view (Just SelectPlayer) (Just pickUp) (Just model.selectedPlayer) model.us
                ]
            , div [ class "them" ]
                [ h1 []
                    [ text "THEM ("
                    , span [ class (String.toLower (toString model.them.side)) ]
                        [ text (toString model.them.side) ]
                    , text ")"
                    ]
                , Team.view Nothing Nothing Nothing model.them
                ]
            ]


gameInProgressView : Model -> List (Html Msg)
gameInProgressView model =
    [ menuPanel model, uiPanel model ]


gameOverView : Model -> List (Html Msg)
gameOverView model =
    let
        ourWins =
            model.roundWinners
                |> List.filter ((==) model.us.side)
                |> List.length

        message =
            if ourWins > 7 then
                "You Won!"
            else
                "You lost."
    in
        [ div [ class "ui afterGame" ]
            [ h1 [] [ text "Game Over!" ]
            , h2 [] [ text message ]
            , button [ onClick Reset ] [ text "Restart?" ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        gameOver =
            (List.length model.roundWinners) >= 15

        subview =
            if gameOver then
                gameOverView
            else
                gameInProgressView
    in
        div []
            ([ historyPanel model
             , KillFeed.view model.killFeed
             ]
                ++ subview model
            )
