module Team exposing (..)

import Player exposing (Player, newPlayer)
import Equipment
import Html exposing (Html)


-- MODEL


type alias Team =
    { players : List Player
    }


ctTeam =
    Team (List.repeat 5 (newPlayer Equipment.CT))


tTeam =
    Team (List.repeat 5 (newPlayer Equipment.T))



-- UPDATE


type Msg
    = PlayerMessage Int Player.Msg


update : Msg -> Team -> Team
update msg team =
    case msg of
        PlayerMessage idx msg ->
            let
                oldPlayers =
                    team.players

                process i player =
                    if i == idx then
                        Player.update msg player
                    else
                        player

                newPlayers =
                    oldPlayers
                        |> List.indexedMap process
            in
                { team | players = newPlayers }



-- VIEW


view : (Msg -> msg) -> Team -> Html msg
view msg team =
    let
        viewPlayer i p =
            Player.view (\a -> msg <| (PlayerMessage i a)) p
    in
        Html.div []
            (List.indexedMap viewPlayer team.players)
