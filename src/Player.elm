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
    Player 10000 Equipment.CT Nothing Nothing [] [] Nothing



-- UPDATE


type Msg
    = Purchase Equipment
    | MenuSelect (Maybe Submenu)


update : Msg -> Player -> Player
update msg origPlayer =
    case msg of
        Purchase item ->
            let
                newMoney =
                    origPlayer.money - (Equipment.cost item)

                player =
                    { origPlayer | money = newMoney, submenu = Nothing }
            in
                case (Equipment.slot item) of
                    Equipment.Primary ->
                        { player | primary = Just item }

                    Equipment.Secondary ->
                        { player | secondary = Just item }

                    Equipment.GearSlot ->
                        if item == Equipment.VestHelmet then
                            { player | gear = item :: (List.filter ((/=) Equipment.Vest) player.gear) }
                        else
                            { player | gear = item :: player.gear }

                    Equipment.Grenade ->
                        { player | grenades = item :: player.grenades }

        MenuSelect sm ->
            { origPlayer | submenu = sm }



-- VIEW


playerCanUseEquipment : Player -> Equipment -> Bool
playerCanUseEquipment p e =
    case (Equipment.teamRestriction e) of
        Just team ->
            p.team == team

        Nothing ->
            True


playerCanPurchaseEquipment : Player -> Equipment -> Bool
playerCanPurchaseEquipment p e =
    if (Equipment.cost e) > p.money then
        False
    else if (p.primary == Just e) then
        False
    else if (p.secondary == Just e) then
        False
    else if not (playerCanUseEquipment p e) then
        False
    else if (List.member e p.gear) then
        False
    else if (e == Equipment.Vest && List.member Equipment.VestHelmet p.gear) then
        False
    else if ((Equipment.slot e) == Equipment.Grenade) && List.length p.grenades > 3 then
        False
    else if (e /= Equipment.Flash && List.member e p.grenades) then
        False
    else if (e == Equipment.Flash && (List.length (List.filter ((==) Equipment.Flash) p.grenades)) > 1) then
        False
    else
        True


buyMenuFor : (Msg -> msg) -> Player -> Html msg
buyMenuFor msg player =
    let
        canPurchase =
            playerCanPurchaseEquipment player
    in
        case player.submenu of
            Nothing ->
                BuyMenu.viewMenu (\a -> (msg <| MenuSelect <| Just a)) player.team

            Just submenu ->
                BuyMenu.viewSubmenu
                    (msg <| MenuSelect Nothing)
                    (\a -> msg <| Purchase a)
                    canPurchase
                    player.team
                    ((Equipment.listFor submenu) |> List.filter (playerCanUseEquipment player))


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
