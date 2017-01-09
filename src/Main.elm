module Main exposing (..)

import Html exposing (Html)
import Html.Events
import List
import Player
import Equipment
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
    { player1 : Player.Player
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Player.emptyPlayer), Cmd.none )



-- UPDATE


type Msg
    = Player1Msg Player.Msg
    | NoMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Player1Msg msg ->
            let
                player1 =
                    model.player1
            in
                ( { model | player1 = Player.update msg player1 }, Cmd.none )

        NoMsg ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ BuyMenu.view NoMsg 6
        , Player.view Player1Msg model.player1
        ]
