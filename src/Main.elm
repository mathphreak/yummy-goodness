module Main exposing (..)

import Html exposing (Html)
import Html.Events
import List
import Player exposing (Player)
import Equipment
import Team exposing (Team)


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
    }


init : ( Model, Cmd Msg )
init =
    ( Model Team.ctTeam Team.tTeam, Cmd.none )



-- UPDATE


type Msg
    = UsMsg Team.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsMsg msg ->
            let
                us =
                    model.us
            in
                ( { model | us = Team.update msg us }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ Team.view UsMsg model.us
        ]
