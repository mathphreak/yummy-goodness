module Team
    exposing
        ( Team
        , buildTeam
        , Msg(..)
        , update
        , view
        )

import Array exposing (Array)
import Array.Extra
import Player exposing (Player, newPlayer)
import Equipment exposing (Equipment)
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events


-- MODEL


type alias Team =
    { side : Equipment.Side
    , players : Array Player
    , ground : Array Equipment
    }


buildTeam : Equipment.Side -> Array String -> Team
buildTeam side names =
    let
        players =
            names
                |> Array.map (newPlayer side)
    in
        Team side players Array.empty



-- UPDATE


type Msg
    = PlayerMessage Int Player.Msg
    | PickUp Int Int
    | None


getDroppedWeapons : Array Player -> Array Equipment
getDroppedWeapons players =
    players
        |> Array.map .dropped
        |> Array.foldl (++) []
        |> Array.fromList


update : Msg -> Team -> Team
update msg team =
    case msg of
        PlayerMessage idx msg ->
            let
                prePlayers =
                    case (Array.get idx team.players) of
                        Nothing ->
                            team.players

                        Just old ->
                            team.players
                                |> Array.set idx (Player.update msg old)

                droppedWeapons =
                    getDroppedWeapons prePlayers

                newGround =
                    Array.append team.ground droppedWeapons

                newPlayers =
                    prePlayers
                        |> Array.map (\p -> { p | dropped = [] })
            in
                { team | players = newPlayers, ground = newGround }

        PickUp playerIdx weaponIdx ->
            let
                weapon =
                    Array.get weaponIdx team.ground

                newGround =
                    case weapon of
                        Just _ ->
                            Array.Extra.removeAt weaponIdx team.ground

                        Nothing ->
                            team.ground

                nextMessage =
                    case weapon of
                        Just w ->
                            PlayerMessage playerIdx (Player.PickUp w)

                        Nothing ->
                            None
            in
                update nextMessage { team | ground = newGround }

        None ->
            team



-- VIEW


viewGround : Maybe (Int -> msg) -> Team -> List (Html msg)
viewGround pickUp team =
    let
        viewItem i e =
            case pickUp of
                Just p ->
                    Html.button [ Html.Events.onClick (p i) ] [ Html.text (Equipment.toString e) ]

                Nothing ->
                    Html.div [] [ Html.text (Equipment.toString e) ]

        empty =
            Array.isEmpty team.ground
    in
        if empty then
            []
        else
            [ Html.div [ class "ground" ]
                [ Html.p [] [ Html.text "On ground: " ]
                , Html.div [] (Array.toList (Array.indexedMap viewItem team.ground))
                ]
            ]


view : Maybe (Int -> msg) -> Maybe (Int -> msg) -> Maybe Int -> Team -> Html msg
view click pickUp selected team =
    let
        viewPlayer i p =
            Player.view (click |> Maybe.map (\m -> m i)) (selected == Just i) p
    in
        Html.div [ class "team" ]
            ((Array.toList (Array.indexedMap viewPlayer team.players))
                ++ (viewGround pickUp team)
            )
