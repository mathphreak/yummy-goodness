module Team exposing (..)

import Player exposing (Player, newPlayer)
import Equipment
import Html exposing (Html)
import Html.Attributes exposing (..)


-- MODEL


type alias Team =
    { players : List Player
    }


buildTeam : Equipment.Side -> List String -> Team
buildTeam side names =
    let
        players =
            names
                |> List.map (newPlayer side)
    in
        Team players


getPlayer : Int -> Team -> Maybe Player
getPlayer i team =
    team.players
        |> List.take (i + 1)
        |> List.drop i
        |> List.head



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


view : Maybe (Int -> msg) -> Maybe Int -> Team -> Html msg
view msg selected team =
    let
        viewPlayer i p =
            Player.view (msg |> Maybe.map (\m -> m i)) (selected == Just i) p
    in
        Html.div [ class "team" ]
            (List.indexedMap viewPlayer team.players)
