module Main exposing (..)

import Html exposing (Html)
import Html.Events
import List
import Player
import Equipment
import Equipment.Lists


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
    ( Model (Player.Player 0 Nothing Nothing Nothing [] []), Cmd.none )



-- UPDATE


type Msg
    = Purchase Equipment.Primary


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Purchase item ->
            let
                player1 =
                    model.player1
            in
                ( { model | player1 = { player1 | primary = Just item } }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        enlist a =
            [ a ]
    in
        Html.div []
            [ Html.text (toString model.player1.money)
            , Html.ul [] [ Html.li [] <| enlist <| Html.text <| toString model.player1.primary ]
            , Html.ul []
                (List.map
                    (\e ->
                        Html.li []
                            [ Html.button [ Html.Events.onClick (Purchase e) ] [ Html.text ("Purchase " ++ toString e) ]
                            ]
                    )
                    Equipment.Lists.rifle
                )
            ]
