module Player exposing (..)

import Equipment exposing (Equipment, Team, Submenu)
import Html exposing (Html)
import Html.Events
import String
import BuyMenu


-- MODEL


type alias Player =
    { money : Int
    , team : Team
    , secondary : Maybe Equipment
    , primary : Maybe Equipment
    , gear : List Equipment
    , grenades : List Equipment
    , submenu : Maybe Submenu
    }


emptyPlayer =
    Player 0 Equipment.CT Nothing Nothing [] [] Nothing



-- UPDATE


type Msg
    = Purchase Equipment
    | MenuSelect (Maybe Submenu)


update : Msg -> Player -> Player
update msg player =
    case msg of
        Purchase item ->
            case (Equipment.slot item) of
                Equipment.Primary ->
                    { player | primary = Just item, submenu = Nothing }

                Equipment.Secondary ->
                    { player | secondary = Just item, submenu = Nothing }

                Equipment.GearSlot ->
                    { player | gear = item :: player.gear, submenu = Nothing }

                Equipment.Grenade ->
                    { player | grenades = item :: player.grenades, submenu = Nothing }

        MenuSelect sm ->
            { player | submenu = sm }



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


buyMenuFor : (Msg -> msg) -> Player -> Html msg
buyMenuFor msg player =
    case player.submenu of
        Nothing ->
            BuyMenu.viewMenu (\a -> (msg <| MenuSelect <| Just a))

        Just submenu ->
            BuyMenu.viewSubmenu (\a -> msg <| Purchase a) ((Equipment.listFor submenu) |> List.filter (playerCanUseEquipment player))


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
        , buyMenuFor msg player
        ]
