module Team
    exposing
        ( Team
        , buildTeam
        , Msg(PlayerMessage)
        , update
        , view
        )

import Array exposing (Array)
import Player exposing (Player, newPlayer)
import Equipment
import Html exposing (Html)
import Html.Attributes exposing (..)


-- MODEL


type alias Team =
    { side : Equipment.Side
    , players : Array Player
    }


buildTeam : Equipment.Side -> Array String -> Team
buildTeam side names =
    let
        players =
            names
                |> Array.map (newPlayer side)
    in
        Team side players



-- UPDATE


type Msg
    = PlayerMessage Int Player.Msg


update : Msg -> Team -> Team
update msg team =
    case msg of
        PlayerMessage idx msg ->
            let
                newPlayers =
                    case (Array.get idx team.players) of
                        Nothing ->
                            team.players

                        Just old ->
                            team.players
                                |> Array.set idx (Player.update msg old)
            in
                { team | players = newPlayers }



-- VIEW


view : Maybe (Int -> msg) -> Maybe Int -> Team -> Html msg
view msg selected team =
    let
        viewPlayer i p =
            Player.view (msg |> Maybe.map (\m -> m i)) (selected == Just i) p
    in
        Html.div [ class "team" ]
            (Array.toList (Array.indexedMap viewPlayer team.players))
