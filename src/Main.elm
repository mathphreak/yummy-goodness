module Main exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import List
import Player exposing (Player)
import Equipment
import Team exposing (Team)
import Random
import BotNames
import BuyMenu


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
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Team []) (Team []) Nothing, Random.generate RngLoad (Random.map2 (,) (BotNames.pickNames 10) Random.bool) )



-- UPDATE


type Msg
    = RngLoad ( List String, Bool )
    | UsMsg Team.Msg
    | SelectPlayer Int


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
                    List.take 5 names

                theirSide =
                    if weAreCT then
                        Equipment.T
                    else
                        Equipment.CT

                theirNames =
                    List.drop 5 names
            in
                ( Model (Team.buildTeam ourSide ourNames) (Team.buildTeam theirSide theirNames) Nothing
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        menu =
            case model.selectedPlayer of
                Just i ->
                    case (Team.getPlayer i model.us) of
                        Just p ->
                            Player.buyMenuFor (\a -> (UsMsg <| Team.PlayerMessage i <| a)) p

                        Nothing ->
                            Html.p [] [ Html.text "Select a player!" ]

                Nothing ->
                    Html.p [] [ Html.text "Select a player!" ]
    in
        Html.div []
            [ Html.div [ Html.Attributes.class "ui between-rounds" ]
                [ Html.div [ Html.Attributes.class "us" ]
                    [ Html.h1 [] [ Html.text "US" ]
                    , Team.view (Just SelectPlayer) model.selectedPlayer model.us
                    ]
                , menu
                , Html.div [ Html.Attributes.class "them" ]
                    [ Html.h1 [] [ Html.text "THEM" ]
                    , Team.view Nothing Nothing model.them
                    ]
                ]
            ]
