module Main exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
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
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { us : Team
    , them : Team
    , selectedPlayer : Maybe Int
    , killFeed : KillFeed
    , roundWinners : List Equipment.Side
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        (Team.buildTeam Equipment.CT Array.empty)
        (Team.buildTeam Equipment.CT Array.empty)
        Nothing
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
                ( Model (Team.buildTeam ourSide ourNames) (Team.buildTeam theirSide theirNames) Nothing [] []
                , Cmd.none
                )

        UsMsg msg ->
            let
                us =
                    model.us
            in
                ( { model | us = Team.update msg us }, Cmd.none )

        SelectPlayer i ->
            ( { model | selectedPlayer = Just i }, Cmd.none )

        BeginSimulation ->
            ( model, Random.generate EndSimulation (Simulation.simulate ( model.us, model.them, model.roundWinners )) )

        EndSimulation ( us, them, killFeed, winner ) ->
            ( { model
                | us = us
                , them = them
                , selectedPlayer = Nothing
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
    model.selectedPlayer
        |> Maybe.andThen (\i -> Maybe.map2 (,) (Just i) (Array.get i model.us.players))


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
        [ Html.li [ Html.Attributes.class "summary" ]
            [ Html.span [ Html.Attributes.class (Tuple.first classes) ] [ Html.text (toString wins) ]
            , Html.text "-"
            , Html.span [ Html.Attributes.class (Tuple.second classes) ] [ Html.text (toString losses) ]
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

        elementFor i ( class, letter ) =
            Html.li [ Html.Attributes.class class ] [ Html.span [] [ Html.text letter ], Html.small [] [ Html.text (toString (i + 1)) ] ]
    in
        winners
            |> List.map textFor
            |> List.indexedMap elementFor


nextRoundButton : List Equipment.Side -> List (Html Msg)
nextRoundButton winners =
    if (List.length winners < 15) then
        [ Html.li [ Html.Attributes.class "next" ]
            [ Html.button [ Html.Attributes.type_ "button", Html.Events.onClick BeginSimulation ] [ Html.text "NEXT" ]
            ]
        ]
    else
        []


historyPanel : Model -> Html Msg
historyPanel model =
    Html.div [ Html.Attributes.class "history" ]
        [ Html.ul [ Html.Attributes.class "rounds" ]
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
                    [ Html.p [] [ Html.text "Select a player!" ] ]

        actions =
            case (getSelectedPlayer model) of
                Just ( i, p ) ->
                    [ Player.actionsFor (\a -> (UsMsg <| Team.PlayerMessage i <| a)) p ]

                Nothing ->
                    []
    in
        Html.div [ Html.Attributes.class "menus" ] (menu ++ actions)


uiPanel : Model -> Html Msg
uiPanel model =
    let
        pickUp =
            model.selectedPlayer
                |> Maybe.map (\i -> (\a -> UsMsg <| Team.PickUp i a))
    in
        Html.div [ Html.Attributes.class "ui inGame" ]
            [ Html.div [ Html.Attributes.class "us" ]
                [ Html.h1 []
                    [ Html.text "US ("
                    , Html.span [ Html.Attributes.class (String.toLower (toString model.us.side)) ]
                        [ Html.text (toString model.us.side) ]
                    , Html.text ")"
                    ]
                , Team.view (Just SelectPlayer) pickUp model.selectedPlayer model.us
                ]
            , Html.div [ Html.Attributes.class "them" ]
                [ Html.h1 []
                    [ Html.text "THEM ("
                    , Html.span [ Html.Attributes.class (String.toLower (toString model.them.side)) ]
                        [ Html.text (toString model.them.side) ]
                    , Html.text ")"
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
        [ Html.div [ Html.Attributes.class "ui afterGame" ]
            [ Html.h1 [] [ Html.text "Game Over!" ]
            , Html.h2 [] [ Html.text message ]
            , Html.button [ Html.Events.onClick Reset ] [ Html.text "Restart?" ]
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
        Html.div []
            ([ historyPanel model
             , KillFeed.view model.killFeed
             , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
             ]
                ++ subview model
            )
