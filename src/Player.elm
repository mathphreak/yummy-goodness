module Player exposing (..)

import Equipment exposing (Equipment, Team)
import Html exposing (Html)
import Html.Events
import String


-- MODEL


type alias Player =
    { money : Int
    , team : Team
    , secondary : Maybe Equipment
    , primary : Maybe Equipment
    , gear : List Equipment
    , grenades : List Equipment
    }


emptyPlayer =
    Player 0 Equipment.CT Nothing Nothing [] []



-- UPDATE


type Msg
    = Purchase Equipment


update : Msg -> Player -> Player
update msg player =
    case msg of
        Purchase item ->
            case (Equipment.slot item) of
                Equipment.Primary ->
                    { player | primary = Just item }

                Equipment.Secondary ->
                    { player | secondary = Just item }

                Equipment.Gear ->
                    { player | gear = item :: player.gear }

                Equipment.Grenade ->
                    { player | grenades = item :: player.grenades }



-- VIEW


type alias Options =
    { pistols : List Equipment
    , heavy : List Equipment
    , smgs : List Equipment
    , rifles : List Equipment
    , gear : List Equipment
    , grenades : List Equipment
    }


playerCanUseEquipment : Player -> Equipment -> Bool
playerCanUseEquipment p e =
    case (Equipment.teamRestriction e) of
        Just team ->
            p.team == team

        Nothing ->
            True


options : Player -> Options
options player =
    let
        pistols =
            .pistols Equipment.lists
                |> List.filter (playerCanUseEquipment player)

        heavy =
            .heavy Equipment.lists
                |> List.filter (playerCanUseEquipment player)

        smgs =
            .smgs Equipment.lists
                |> List.filter (playerCanUseEquipment player)

        rifles =
            .rifles Equipment.lists
                |> List.filter (playerCanUseEquipment player)

        gear =
            .gear Equipment.lists
                |> List.filter (playerCanUseEquipment player)

        grenades =
            .grenades Equipment.lists
                |> List.filter (playerCanUseEquipment player)
    in
        Options pistols heavy smgs rifles gear grenades


purchaseButton : (Msg -> msg) -> Equipment -> Html msg
purchaseButton msg e =
    Html.button
        [ Html.Events.onClick (msg (Purchase e)) ]
        [ Html.text ("Purchase " ++ Equipment.toString e) ]


purchaseSubmenu : (Msg -> msg) -> List Equipment -> Html msg
purchaseSubmenu msg l =
    Html.ul []
        (List.map
            (\e -> Html.li [] [ purchaseButton msg e ])
            l
        )


view : (Msg -> msg) -> Player -> Html msg
view msg player =
    Html.div []
        [ Html.text (toString player.money)
        , Html.ul []
            [ Html.li [] [ Html.text (Maybe.withDefault "nothing" (player.primary |> Maybe.map Equipment.toString)) ]
            , Html.li [] [ Html.text (Maybe.withDefault "nothing" (player.secondary |> Maybe.map Equipment.toString)) ]
            , Html.li [] [ Html.text (player.gear |> List.map Equipment.toString |> String.join ", ") ]
            , Html.li [] [ Html.text (player.grenades |> List.map Equipment.toString |> String.join ", ") ]
            ]
        , purchaseSubmenu msg (.pistols (options player))
        , purchaseSubmenu msg (.heavy (options player))
        , purchaseSubmenu msg (.smgs (options player))
        , purchaseSubmenu msg (.rifles (options player))
        , purchaseSubmenu msg (.gear (options player))
        , purchaseSubmenu msg (.grenades (options player))
        ]
